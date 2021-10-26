#include <map>
#include <math.h>       /* pow */

#include <vector>
#include <algorithm>    // std::sort, std::stable_sort

#include <chrono>
#include <iostream>

#pragma warning(disable:4996)

using namespace std;

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
//#define MAXBIT 30
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */
#define TOTAL_PIXELS 262144		/*max number of rgb in 1D 512*512 = 262144*/

#define LAMBDA_FINAL 0.01	/*decay constant final*/
#define MAX_ITERS 100		// maximum number of iteration, default 100
#define CONV_THRESH 0.01	//convergence threshold, default ε = 0.001

//testing
//#define num_pixels 262144	//testing 512*512=262144


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
double calcMSE(const RGB_Image* img, RGB_Cluster* clusters, int k);

/* Color quantization using the batch k-means algorithm */
void batch_kmeans(const RGB_Image* img, const int k, RGB_Cluster* clusters)
{
	//step 1: initializing random centroids (aldready done in main)

	//Declare variables
	int iter_cnt = 0;
	double SSE = 0;
	RGB_Pixel* centers_temp = (RGB_Pixel*)malloc(k * sizeof(RGB_Pixel)); /*temporary centers of clusters in current iteration*/
	int num_pts_change = 0;
	int* pixels_member = (int*)malloc(img->size * sizeof(int));
	//initialize -1 as cluster membership for each pixel
	for (int i = 0; i < img->size; i++) {
		pixels_member[i] = -1;
	}

	/*step2: loop and stop until all pixels stop changing their memberships */
	while (true)
	{
		//reset SSE and reset number of points change
		num_pts_change = 0;
		SSE = 0;

		//reset the size of each cluster and centers_temp	
		for (int i = 0; i < k; i++) {
			clusters[i].size = 0;
		}
		//reset centers_temp
		for (int i = 0; i < k; i++) {
			RGB_Pixel* center_temp = &centers_temp[i];
			center_temp->red = 0;
			center_temp->green = 0;
			center_temp->blue = 0;
		}

		/*step3: Assign each pixel to nearerst cluster*/
		//Iterate over each pixel in the img
		for (int i = 0; i < img->size; i++) {	//img->size
			//3.1 calculate distance of each pixel to each centroid using Squared Euclidean distance
			RGB_Pixel* pixel = &img->data[i]; // cache the pixel to limit the CPU keep accessing the memory 6 times

			//cout << "\nrgb is: " << pixel->red << "," << pixel->green << "," << pixel->blue << endl; //testing
			double min_dist = TOTAL_PIXELS; //initiazize a big number
			int min_index = -1; //initiazize a big number
			for (int j = 0; j < k; j++) {
				RGB_Cluster* cluster = &clusters[j];
				double delta_red = pixel->red - cluster->center.red;
				double delta_green = pixel->green - cluster->center.green;
				double delta_blue = pixel->blue - cluster->center.blue;
				double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
				//cout << "distance is: " << dist << endl; //testing
				if (dist < min_dist) {
					min_dist = dist;
					min_index = j;
				}
			}
			//cout << "Nearest centroid is: " << min_index << endl; //testing
			//3.2 assign each pixel to the nearest centroid
			centers_temp[min_index].red += pixel->red;
			centers_temp[min_index].green += pixel->green;
			centers_temp[min_index].blue += pixel->blue;
			clusters[min_index].size += 1;
			//update pixels_member array and num_pts_change
			if (pixels_member[i] != min_index) {
				pixels_member[i] = min_index;
				num_pts_change++;
			}

			//calculate SSE
			SSE += min_dist;
		}

		//cout << "num_pts_change is: " << num_pts_change << endl; //testing

		//step4: Recompute the cetroid of each cluster
		for (int i = 0; i < k; i++) {
			RGB_Cluster* cluster = &clusters[i];
			RGB_Pixel* center_temp = &centers_temp[i];
			if (cluster->size != 0) { // allow the center to remain the same if the cluster size is zero
				cluster->center.red = center_temp->red / cluster->size;
				cluster->center.green = center_temp->green / cluster->size;
				cluster->center.blue = center_temp->blue / cluster->size;
			}
		}
		printf("Iteration %d: SSE = %0.4f\n", iter_cnt + 1, SSE);

		//Step 5: If points stop changing their memberships, stop the loop
		if (num_pts_change == 0) {
			break;
		}
		iter_cnt++;
	}

	// Free the memory 
	free(centers_temp);
}

/* Color quantization using the batch neural gas algorithm */
void batch_neural_gas(const RGB_Image* img, const int k, RGB_Cluster* clusters) {
	//step 1: initializing random centroids (aldready done in main)

	//Declare variables
	double* LUT_h_func = (double*)malloc(k * sizeof(double)); //look up table for h_function
	vector<double> dist_array; //distances array
	double* weights = (double*)malloc(k * sizeof(double)); /*temporary weights*/
	RGB_Pixel* centers_temp = (RGB_Pixel*)malloc(k * sizeof(RGB_Pixel)); /*temporary centers of clusters in current iteration*/
	

	double lambda_initial = k / 2.0;
	double lambda;

	double current_cost = 0;
	double relative_cost = 1; //initialized with anything greater than CONV_THRESH(0.01)
	double previous_cost = 0;

	int iter_cnt = 0; //iteration counter
	double SSE = 0; //sum squared error

	/*step2: loop and stop until convergence threashold is reached or max iteration is reached*/
	while (iter_cnt < MAX_ITERS && relative_cost > CONV_THRESH) //MAX_ITERS 
	{
		//iter_cnt < MAX_ITERS&& relative_cost > CONV_THRESH

		//First, calculate the decay constant for this iteration
		//double test = pow((LAMBDA_FINAL / lambda_initial), (iter_cnt * 1.0 / MAX_ITERS));
		lambda = lambda_initial * pow((LAMBDA_FINAL / lambda_initial), (iter_cnt / (double)MAX_ITERS)); //20.0  //MAX_ITERS
		cout << "decay constant is: " << lambda << endl; //testing

		//Look up table for h_function
		for (int j = 0; j < k; j++) {
			LUT_h_func[j] = exp(-j / lambda);
		}

		//reset centers_temp and weights
		for (int i = 0; i < k; i++) {
			RGB_Pixel* center_temp = &centers_temp[i];
			center_temp->red = 0;
			center_temp->green = 0;
			center_temp->blue = 0;
			weights[i] = 0;
		}

		//reset current_cost
		current_cost = 0;

		/*step3: Compute Euclidean distance and sorted the distance for all prototype*/
		//Iterate over each pixel in the img		//img->size //testing with 10 pixels
		for (int i = 0; i < img->size; i++) {
			
			RGB_Pixel* pixel = &img->data[i]; // cache the pixel to limit the CPU keep accessing the memory
			//cout << "\nrgb is: " << pixel->red << "," << pixel->green << "," << pixel->blue << endl; //testing
			
			//reset dist_array
			dist_array.clear();

			//3.1 calculate distance of this pixel to each centroid using Squared Euclidean distance  \
			and add it to distances array
			for (int j = 0; j < k; j++) {
				RGB_Cluster* cluster = &clusters[j];
				double delta_red = pixel->red - cluster->center.red;
				double delta_green = pixel->green - cluster->center.green;
				double delta_blue = pixel->blue - cluster->center.blue;
				double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
				dist_array.push_back(dist);
				////cout << "distance is: " << dist << endl; //testing
			}

			//3.2 Sort the distances and get the rank of prototypes
			vector<int> index_rank(dist_array.size(), 0);
			for (int i = 0; i != index_rank.size(); i++) {
				index_rank[i] = i;
			}
			sort(index_rank.begin(), index_rank.end(),
				[&](const int& a, const int& b) {
					return (dist_array[a] < dist_array[b]);
				}
			);
			
			//3.3: calculate the cost function and updating for prototypes
			for (int j = 0; j < k; j++) { //for each cluster
				//3.3.1 compute the updating for prototypes according to rank to each centroid for this pixel
				int index = index_rank[j];
				double h_func = LUT_h_func[j]; //cache it for faster
				centers_temp[index].red += h_func * pixel->red;
				centers_temp[index].green += h_func * pixel->green;
				centers_temp[index].blue += h_func * pixel->blue;
				weights[index] += h_func;

				//3.3.2: calculate the cost function
				current_cost += h_func * dist_array[index]; //testing //1.5258789062500000e-05 //probability(img, *pixel)
			}
		}	//end of the loop to iterate over all pixels

		//Step4: Recompute the cetroid of each cluster
		for (int j = 0; j < k; j++) {
			RGB_Cluster* cluster = &clusters[j];
			RGB_Pixel* center_temp = &centers_temp[j];
			//printf("update_red green blue are: %f , %f, %f", center_temp->green, center_temp->red, center_temp->blue); //testing
			cluster->center.red = center_temp->red / weights[j];
			cluster->center.green = center_temp->green / weights[j];
			cluster->center.blue = center_temp->blue / weights[j];
		}

		//Calculate relative cost
		if (iter_cnt == 0) {
			relative_cost = 1; // anything > CONV_THRESH (0.001)
		}
		else {
			relative_cost = (previous_cost - current_cost) / current_cost;
		}
		
		
		printf("Iteration %d: cost = %.9f\n", iter_cnt + 1, current_cost);
		printf("Iteration %d: relative_cost = %.9f\n", iter_cnt + 1, relative_cost);

		//testing SSE
		printf("Iteration %d: MSE = %0.4f\n", iter_cnt + 1, calcMSE(img, clusters, k));
		printf("----------------End of Iteration %d--------------\n\n", iter_cnt + 1);
		
		//Update previous_cost and iteration counter
		previous_cost = current_cost; //update previous cost
		iter_cnt++; //update counter
	}

	//testing clusters' centers after the algorithm
	cout << "Final clusters' centers are: " << endl;
	for (int i = 0; i < k; i++) {
		//printf("Cluster %d's size is %d \n", i, *(&clusters->size));
		//printf("Cluster %d's center is %d %d %d \n", i, (cluster+i)->center.red, (cluster + i)->center.green, (cluster + i)->center.blue);
		cout << "Cluster " << i << "'s center is: " << (clusters + i)->center.red << "," << (clusters + i)->center.green << "," << (clusters + i)->center.blue << endl;
	}
	// Free the memory 
	free(centers_temp);
	free(weights);
	free(LUT_h_func);
}

/*Save new image by assigning each pixel to the its belonging cluster's centroid*/
void save_new_img(const RGB_Image* img, RGB_Cluster* clusters, int k) {
	//Iterate over each pixel in the img
	for (int i = 0; i < img->size; i++) { //loop to go over all pixels //img->size
		RGB_Pixel* pixel = &img->data[i];
		//cout << "\nrgb is: " << pixel->red << "," << pixel->green << "," << pixel->blue << endl; //testing
		double min_dist = 1000000; //initiazize a big number
		int min_index = -1; //initiazize a big number
		for (int j = 0; j < k; j++) { //distance between the pixel with each centroid
			RGB_Cluster* cluster = &clusters[j];
			double delta_red = pixel->red - cluster->center.red;
			double delta_green = pixel->green - cluster->center.green;
			double delta_blue = pixel->blue - cluster->center.blue;
			double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
			//cout << "distance is: " << dist << endl; //testing
			if (dist < min_dist) {
				min_dist = dist;
				min_index = j;
			}
		}

		//save the pixel with the color of the centroid of its cluter
		RGB_Cluster* nearest_cluster = &clusters[min_index];
		pixel->red = nearest_cluster->center.red;
		pixel->green = nearest_cluster->center.green;
		pixel->blue = nearest_cluster->center.blue;
	}
}

/*Calculate Mean Squared Error (MSE)*/
double calcMSE(const RGB_Image* img, RGB_Cluster* clusters, int k) {
	double MSE = 0;
	double SSE = 0;
	//Iterate over each pixel in the img
	for (int i = 0; i < img->size; i++) {	//img->size
		//3.1 calculate distance of each pixel to each centroid using Squared Euclidean distance
		RGB_Pixel* pixel = &img->data[i];

		//cout << "\nrgb is: " << pixel->red << "," << pixel->green << "," << pixel->blue << endl; //testing
		double min_dist = 1000000; //initiazize a big number
		int min_index = -1; //initiazize a big number
		for (int j = 0; j < k; j++) { //distance between the pixel with each centroid
			RGB_Cluster* cluster = &clusters[j];
			double delta_red = pixel->red - cluster->center.red;
			double delta_green = pixel->green - cluster->center.green;
			double delta_blue = pixel->blue - cluster->center.blue;
			double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
			//cout << "distance is: " << dist << endl; //testing
			if (dist < min_dist) {
				min_dist = dist;
				min_index = j;
			}
		}
		//calculate SSE
		SSE += min_dist;
	}
	//printf("SSE is: %f \n", SSE); //testing
	MSE = SSE / img->size;
	return MSE;
}


void free_img(const RGB_Image* img) {
	/* Free Image Data*/
	free(img->data);

	/* Free Image Pointer*/
	delete(img);
}


int main(int argc, char* argv[])
{
	const char* filename;	//"sample_image.ppm" /* Filename Pointer*/ 
	int k;				// Number of clusters

	//const char* filename = "sample_image.ppm";	//"sample_image.ppm" /* Filename Pointer*/ 
	//int k = 3;		//5			/* Number of clusters*/

	RGB_Image* img;
	//RGB_Image* out_img;//not sure if we need it?
	RGB_Cluster* clusters;

	if (argc == 3) {
		/* Image filename */
		filename = argv[1];

		/* k, number of clusters */
		k = atoi(argv[2]); // atoi function converts a string to an integer

	}
	else if (argc > 3) {
		printf("Too many arguments supplied.\n");
		return 0;
	}
	else {
		printf("Two arguments expected: image filename and number of clusters.\n");
		return 0;
	}

	srand(time(NULL));

	/* Print Args*/
	printf("%s %d\n", filename, k);

	/* Read Image*/
	img = read_PPM(filename);

	/* Test Batch Neural Gas*/
	/* Start Timer*/
	auto start = std::chrono::high_resolution_clock::now();
	cout << endl;
	/* Initialize centers */
	clusters = gen_rand_centers(img, k);
	//batch_kmeans(img, k, clusters);
	batch_neural_gas(img, k, clusters);

	//calculate Mean Squared Error (MSE)
	double MSE = calcMSE(img, clusters, k);
	printf("Mean Squared Error (MSE) is: %f \n", MSE);

	//save new image with new clusters found
	save_new_img(img, clusters, k);

	//Make new image by writing to ppm file
	write_PPM(img, "outputting_img.ppm");

	cout << "\n------------------Finished running--------------------" << endl << endl;
	/* Stop Timer*/
	auto stop = std::chrono::high_resolution_clock::now();
	//auto stop = high_resolution_clock::now();

	/* Execution Time*/
	//auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

	std::chrono::duration<double> diff = stop - start;

	std::cout << "Time to run is: " << diff.count() << " s\n";

	free(clusters);
	free_img(img);

	return 0;
}

