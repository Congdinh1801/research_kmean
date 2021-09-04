#include <chrono>
#include <climits>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <math.h>
#include <string.h>
#include <cstdlib>
#include <time.h>
#include <float.h>
#include <iomanip>
#include <vector>


#include <stdio.h>
#include <stdlib.h>

#pragma warning(disable:4996)

using namespace std;
using namespace std::chrono;

typedef unsigned char uchar;
typedef unsigned long ulong;

/* RGB_Pixel: data type to hold RGB value of a pixel;
struct {double red, green, blue;}*/
typedef struct
{
	double red, green, blue;
} RGB_Pixel;

/* RGB_Image: data type to hold image data
struct (int width, int height, int size, RGB_Pixel* data) */
typedef struct
{
	int width, height;
	int size;
	RGB_Pixel* data;
} RGB_Image;

/* RGB_Cluster: data type to hold cluster data;
struct {int size; RGB_Pixel center;} */
typedef struct
{
	int size;
	RGB_Pixel center;
} RGB_Cluster;

/* Mersenne Twister related constants */
#define N 624
#define M 397
#define MAXBIT 30
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */
#define MAX_RGB_DIST 195075		/*max number of rgb in 1D 255*255*3 = 195075*/
#define MAX_ITERS 100		/*default 100 I: maximum number of iterations in a run*/
#define MAX_RUN 5	/*DEFAULT IS 100*/
#define T 0.000001 /*(convergence threshold)*/

//my defined number
#define num_pixels 262144	


static ulong mt[N]; /* the array for the state vector  */
static int mti = N + 1; /* mti == N + 1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
void init_genrand(ulong s)
{
	mt[0] = s & 0xffffffffUL;
	for (mti = 1; mti < N; mti++)
	{
		mt[mti] =
			(1812433253UL * (mt[mti - 1] ^ (mt[mti - 1] >> 30)) + mti);
		/* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
		/* In the previous versions, MSBs of the seed affect   */
		/* only MSBs of the array mt[].                        */
		/* 2002/01/09 modified by Makoto Matsumoto             */
		mt[mti] &= 0xffffffffUL;
		/* for >32 bit machines */
	}
}

ulong genrand_int32(void)
{
	ulong y;
	static ulong mag01[2] = { 0x0UL, MATRIX_A };
	/* mag01[x] = x * MATRIX_A  for x = 0, 1 */

	if (mti >= N)
	{ /* generate N words at one time */
		int kk;

		if (mti == N + 1)
		{
			/* if init_genrand ( ) has not been called, */
			init_genrand(5489UL); /* a default initial seed is used */
		}

		for (kk = 0; kk < N - M; kk++)
		{
			y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
			mt[kk] = mt[kk + M] ^ (y >> 1) ^ mag01[y & 0x1UL];
		}

		for (; kk < N - 1; kk++)
		{
			y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
			mt[kk] = mt[kk + (M - N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
		}

		y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
		mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ mag01[y & 0x1UL];
		mti = 0;
	}

	y = mt[mti++];

	/* Tempering */
	y ^= (y >> 11);
	y ^= (y << 7) & 0x9d2c5680UL;
	y ^= (y << 15) & 0xefc60000UL;
	y ^= (y >> 18);

	return y;
}

double genrand_real2(void)
{
	return genrand_int32() * (1.0 / 4294967296.0);
	/* divided by 2^32 */
}

/* Function for generating a bounded random integer between 0 and RANGE */
/* Source: http://www.pcg-random.org/posts/bounded-rands.html */
uint32_t bounded_rand(const uint32_t range)
{
	uint32_t x = genrand_int32();
	uint64_t m = ((uint64_t)x) * ((uint64_t)range);
	uint32_t l = (uint32_t)m;

	if (l < range)
	{
		uint32_t t = range;  //-range

		if (t >= range)
		{
			t -= range;
			if (t >= range)
			{
				t %= range;
			}
		}

		while (l < t)
		{
			x = genrand_int32();
			m = ((uint64_t)x) * ((uint64_t)range);
			l = (uint32_t)m;
		}
	}

	return m >> 32; // m = m / (2^32)
}

/*read image
* returntype: RGB_Image*
* RGB_Image: struct RGB_Image (int w, int h, int size, RGB_Pixel* data)
*/
RGB_Image* read_PPM(const char* filename)
{
	uchar byte;
	char buff[16];
	int c, max_rgb_val, i = 0;
	FILE* fp;
	RGB_Pixel* pixel;
	RGB_Image* img;

	fp = fopen(filename, "rb");
	if (!fp)
	{
		fprintf(stderr, "Unable to open file '%s'!\n", filename);
		exit(EXIT_FAILURE);
	}

	/* read image format */
	if (!fgets(buff, sizeof(buff), fp))
	{
		perror(filename);
		exit(EXIT_FAILURE);
	}

	/*check the image format to make sure that it is binary */
	if (buff[0] != 'P' || buff[1] != '6')
	{
		fprintf(stderr, "Invalid image format (must be 'P6')!\n");
		exit(EXIT_FAILURE);
	}

	img = (RGB_Image*)malloc(sizeof(RGB_Image));
	if (!img)
	{
		fprintf(stderr, "Unable to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	/* skip comments */
	c = getc(fp);
	while (c == '#')
	{
		while (getc(fp) != '\n');
		c = getc(fp);
	}

	ungetc(c, fp);

	/* read image dimensions */
	if (fscanf(fp, "%u %u", &img->width, &img->height) != 2)
	{
		fprintf(stderr, "Invalid image dimensions ('%s')!\n", filename);
		exit(EXIT_FAILURE);
	}

	/* read maximum component value */
	if (fscanf(fp, "%d", &max_rgb_val) != 1)
	{
		fprintf(stderr, "Invalid maximum R, G, B value ('%s')!\n", filename);
		exit(EXIT_FAILURE);
	}

	/* validate maximum component value */
	if (max_rgb_val != 255)
	{
		fprintf(stderr, "'%s' is not a 24-bit image!\n", filename);
		exit(EXIT_FAILURE);
	}

	while (fgetc(fp) != '\n');

	/* allocate memory for pixel data */
	img->size = img->height * img->width;
	img->data = (RGB_Pixel*)malloc(img->size * sizeof(RGB_Pixel));

	if (!img)
	{
		fprintf(stderr, "Unable to allocate memory!\n");
		exit(EXIT_FAILURE);
	}

	/* Read in pixels using buffer */
	while (fread(&byte, 1, 1, fp) && i < img->size)
	{
		pixel = &img->data[i];
		pixel->red = byte;
		fread(&byte, 1, 1, fp);
		pixel->green = byte;
		fread(&byte, 1, 1, fp);
		pixel->blue = byte;
		i++;
	}

	fclose(fp);

	return img;
}

void write_PPM(const RGB_Image* img, const char* filename)
{
	uchar byte;
	FILE* fp;

	fp = fopen(filename, "wb"); // 'b' to  open a file as a binary file, 
								//'w' or write: Create an empty file for output operations. 
								//If a file with the same name already exists, its contents are discarded and 
								//the file is treated as a new empty file.'
	if (!fp)
	{
		fprintf(stderr, "Unable to open file '%s'!\n", filename);
		exit(EXIT_FAILURE);
	}

	fprintf(fp, "P6\n");
	fprintf(fp, "%d %d\n", img->width, img->height);
	fprintf(fp, "%d\n", 255);

	for (int i = 0; i < img->size; i++)
	{
		byte = (uchar)img->data[i].red;
		fwrite(&byte, sizeof(uchar), 1, fp);
		byte = (uchar)img->data[i].green;
		fwrite(&byte, sizeof(uchar), 1, fp);
		byte = (uchar)img->data[i].blue;
		fwrite(&byte, sizeof(uchar), 1, fp);
	}

	fclose(fp);
}

/* Function to generate random cluster centers. */
RGB_Cluster* gen_rand_centers(const RGB_Image* img, const int k) {
	RGB_Pixel rand_pixel;
	RGB_Cluster* cluster;

	cluster = (RGB_Cluster*)malloc(k * sizeof(RGB_Cluster));

	for (int i = 0; i < k; i++) {
		/* Make the initial guesses for the centers, m1, m2, ..., mk */
		rand_pixel = img->data[bounded_rand(img->size)];

		cluster[i].center.red = rand_pixel.red;
		cluster[i].center.green = rand_pixel.green;
		cluster[i].center.blue = rand_pixel.blue;

		/* Set the number of points assigned to k cluster to zero, n1, n2, ..., nk */
		cluster[i].size = 0;

		// cout << "\nCluster Centers: " << cluster[i].center.red << ", " << cluster[i].center.green <<", "<<  cluster[i].center.blue;
	}

	return(cluster);
}

/*
   For application of the batch k-means algorithm to color quantization, see
   M. E. Celebi, Improving the Performance of K-Means for Color Quantization,
   Image and Vision Computing, vol. 29, no. 4, pp. 260-271, 2011.
 */

 /* Color quantization using the batch k-means algorithm */
void batch_kmeans(const RGB_Image* img, const int k,
	const int max_iters, RGB_Cluster* clusters)
{
	//batch_kmeans(img, k, INT_MAX, cluster);
	//printf("Cluster %d's size is %d \n", i, *(&cluster->size));
	////printf("Cluster %d's center is %d %d %d \n", i, (cluster+i)->center.red, (cluster + i)->center.green, (cluster + i)->center.blue);
	//cout << "Cluster " << i << "'s center is: " << (cluster + i)->center.red << "," << (cluster + i)->center.green << "," << (cluster + i)->center.blue << endl << endl;

	
	/*RGB_Pixel two_d[2][2];
	vector<RGB_Pixel> cluster0;
	vector<RGB_Pixel> cluster1;
	vector<RGB_Pixel> cluster2;*/
	//step 1 aldready done

	//Declare variables
	int counter_test = 0;

	////RGB_Pixel centers_temp[3] = {0}; 
	//RGB_Pixel* centers_temp = (RGB_Pixel*)malloc(k * sizeof(RGB_Pixel)); /*temporary centers of clusters in current iteration*/
	///*double dist_cluster[k] = {0};*/
	//double* dist_cluster = (double*)malloc(k * sizeof(double));

	double SSE_past = 0; 
	double SSE = 0; //first one is the past, second one is current
	int size_img = *(&img->size);
	//cout << "size of image is: " << size_img;

	//testing for initial clusters' centers
	//cout << "Initial clusters' centers are: " << endl;
	//for (int i = 0; i < k; i++) {
	//	printf("Cluster %d's size is %d \n", i, *(&clusters->size));
	//	//printf("Cluster %d's center is %d %d %d \n", i, (cluster+i)->center.red, (cluster + i)->center.green, (cluster + i)->center.blue);
	//	cout << "Cluster " << i << "'s center is: " << (clusters + i)->center.red << "," << (clusters + i)->center.green << "," << (clusters + i)->center.blue << endl << endl;
	//}

	while (counter_test < max_iters) /*step2: loop and Stop when relative improvement in SSE(Sum of Squared Error) between two consecutive iterations drops below T, threshold or reached maximum iteration*/
	{
		//RGB_Pixel centers_temp[3] = {0}; 
		RGB_Pixel* centers_temp = (RGB_Pixel*)malloc(k * sizeof(RGB_Pixel)); /*temporary centers of clusters in current iteration*/
		/*double dist_cluster[k] = {0};*/
		double* dist_cluster = (double*)malloc(k * sizeof(double));

		//cout << "Iteration " << counter_test+1 << ": ";
		/*printf("Iteration %d: ", counter_test+1);*/
		//reset SSE
		SSE = 0;
		
		//step3: Assign each pixel to cluster
		//reset the size of each cluster and centers_temp	
		for (int i = 0; i < k; i++) {
			clusters[i].size = 0;
		}
		//reset centers_temp
		for (int i = 0; i < k; i++) {
			centers_temp[i].red = 0;
			centers_temp[i].green = 0;
			centers_temp[i].blue = 0;
		}

		//loop to go over all pixels
		for (int i = 0; i < size_img; i++) {	//size_img
			//3.1 calculate distance of each pixel to each centroid using Squared Euclidean distance
			double r = *(&img->data[i].red);
			double g = *(&img->data[i].green);
			double b = *(&img->data[i].blue);

			//cout << "\nrgb is: " << r << "," << g << "," << b << endl; //testing

			for (int j = 0; j < k; j++) {
				dist_cluster[j] = (r - (clusters + j)->center.red) * (r - (clusters + j)->center.red)
					+ (g - (clusters + j)->center.green) * (g - (clusters + j)->center.green)
					+ (b - (clusters + j)->center.blue) * (b - (clusters + j)->center.blue);
				//cout << "distance is: " << dist_cluster[j] << endl; //testing
			}
			//3.2 assign each pixel to the nearest centroid
			int ind = 0;
			double min = dist_cluster[0];
			for (int i = 0; i < k; i++) {
				if (dist_cluster[i] < min) {
					min = dist_cluster[i];
					ind = i;
				}
			}
			//int ind = std::distance(dist_cluster, max_element(dist_cluster, dist_cluster + sizeof(dist_cluster) / sizeof(dist_cluster[0])));
			//cout << "Nearest centroid is: " << ind << endl; //testing
			centers_temp[ind].red += r;
			centers_temp[ind].green += g;
			centers_temp[ind].blue += b;
			clusters[ind].size += 1;
			//calculate SSE
			SSE += min;			
		}

		//step4: Recompute the cetroid of each cluster
		for (int i = 0; i < k; i++) {
			(clusters + i)->center.red = centers_temp[i].red / clusters[i].size;
			(clusters + i)->center.green = centers_temp[i].green / clusters[i].size;
			(clusters + i)->center.blue = centers_temp[i].blue / clusters[i].size;
		}


		//calculate SSE		
		if (SSE_past == 0) { //first time running
			SSE_past = SSE;
			//cout << "SSE = " << SSE << endl;
			printf("Iteration %d: SSE = %0.4f\n", counter_test + 1, SSE);
			counter_test++;
			//printf("SSE = %0.4f\n", SSE);
			//cout << "Current threshhold is ignored (first iteration) " << endl; //test
			continue;
		}

		double current_T = (SSE_past - SSE) / SSE_past;
		SSE_past = SSE;
		//printf("SSE = %0.4f\n", SSE);
		printf("Iteration %d: SSE = %0.4f\n", counter_test+1, SSE);
		//cout << "relative improvement in SSE = " << current_T << endl;
		if (current_T < T) 
		{
			//cout << "threshhold reached when " << current_T << " < T(" << T << ")"; //another way
			printf("threshhold reached when %.9f < T(%0.6f)", current_T, T); //test
			break;
		}

		////testing 
		//unsigned int size_total = 0;
		//for (int i = 0; i < k; i++) {
		//	if ((clusters + i)->size == 0) {
		//		printf("cluster %d has size 0, skip\n\n", i);
		//		continue;
		//	}
		//	printf("Cluster %d's size is %d \n", i, clusters[i].size);
		//	size_total += clusters[i].size;
		//	cout << "Cluster " << i << "'s center is: " << (clusters + i)->center.red << "," << (clusters + i)->center.green << "," << (clusters + i)->center.blue << endl << endl;
		//}
		//cout << "size_total is: " << size_total << endl;
		//if (num_pixels != size_total) {
		//	printf("the size total is different. Should be 262144\n");
		//}
	

		// Free the memory
		free(dist_cluster);
		// Free the memory
		free(centers_temp);

		counter_test++;
	}
	//testing 
	/*unsigned int size_total = 0;
	cout << "\n\nResulted clusters are: " << endl;
	for (int i = 0; i < k; i++) {
		if ((clusters + i)->size == 0) {
			printf("cluster %d has size 0, skip\n\n", i);
			continue;
		}
		printf("Cluster %d's size is %d \n", i, clusters[i].size);
		size_total += clusters[i].size;
		cout << "Cluster " << i << "'s center is: " << (clusters + i)->center.red << "," << (clusters + i)->center.green << "," << (clusters + i)->center.blue << endl << endl;
	}
	cout << "size_total is: " << size_total << endl;
	if (num_pixels != size_total) {
		printf("the size total is different. Should be 262144\n");
	}*/
	
	//// Free the memory
	//free(dist_cluster);
	//// Free the memory
	//free(centers_temp);
}

void free_img(const RGB_Image* img) {
	/* Free Image Data*/
	free(img->data);

	/* Free Image Pointer*/
	delete(img);
}

int main(int argc, char* argv[])
{
	// To run skeleton.cpp call: 
	//skeleton.cpp F K I T R
	//skeleton.cpp data_file num_clusters Max_run thresh_hold num_runs
	//skeleton.cpp ecoli.txt 8 100 0.000001 3
	//skeleton.cpp ecoli.txt 8 100 0.000001 3
	const char* filename = "sample_image.ppm";	/* Filename Pointer*/
	int k = 5;								/* Number of clusters*/
	RGB_Image* img;
	RGB_Image* out_img;
	RGB_Cluster* cluster;

	//initialized cluster


	//if (argc == 3) {
	//	/* Image filename */
	//	filename = argv[1];

	//	/* k, number of clusters */
	//	k = atoi(argv[2]); // atoi function converts a string to an integer

	//}
	//else if (argc > 3) {
	//	printf("Too many arguments supplied.\n");
	//	return 0;
	//}
	//else {
	//	printf("Two arguments expected: image filename and number of clusters.\n");
	//	return 0;
	//}

	srand(time(NULL));

	/* Print Args*/
	printf("%s %d\n", filename, k);

	/* Read Image*/
	img = read_PPM(filename);

	cout << endl;
	cout << "image is: " << img << endl;
	cout << "img->width:  " << img->width << endl;
	cout << "img->height: " << img->height << endl;
	cout << "img->size: " << img->size << endl;


	/*cout << *(&img->data[num_pixels+5].red) << endl;
	cout << *(&img->data[num_pixels+5].green) << endl;
	cout << *(&img->data[num_pixels+5].blue) << endl << endl;*/

	for (int i = 0; i < 3; i++) {
		cout << i << " pixel: " << *(&img->data[i].red) << "," << *(&img->data[i].green)
			<< "," << *(&img->data[i].blue) << endl;
	}

	cout << "first pixel address is: " << &img->data[0].red << "," << &img->data[0].green << "," << &img->data[0].blue << "," << endl;
	cout << "second pixel address is: " << &img->data[1].red << "," << &img->data[1].green << "," << &img->data[1].blue << "," << endl;


	/* Test Batch K-Means*/
	/* Start Timer*/
	auto start = std::chrono::high_resolution_clock::now();

	/* Initialize centers */
	//cluster = gen_rand_centers(img, k);

	/*testing delete after done*/
	cout << endl;

	//for (int i = 0; i < 3; i++) {
	//	printf("Cluster %d's size is %d \n", i, *(&cluster->size));
	//	//printf("Cluster %d's center is %d %d %d \n", i, (cluster+i)->center.red, (cluster + i)->center.green, (cluster + i)->center.blue);
	//	cout << "Cluster " << i << "'s center is: " << (cluster+i)->center.red << "," << (cluster+i)->center.green << "," << (cluster+i)->center.blue << endl << endl;
	//}

	/* Implement Batch K-means*/
	for (int i = 0; i < MAX_RUN; i++) {
		printf("\nRun %d\n-------\n", i+1);
		cluster = gen_rand_centers(img, k);
		batch_kmeans(img, k, MAX_ITERS, cluster);
		cout << "\n------------------Finished running--------------------" << endl << endl;
	}
	

	/* Stop Timer*/
	auto stop = std::chrono::high_resolution_clock::now();
	//auto stop = high_resolution_clock::now();

	/* Execution Time*/
	auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

	std::chrono::duration<double> diff = stop - start;

	std::cout << "Time to run is: " << diff.count() << " s\n";

	free(cluster);

	return 0;
}

