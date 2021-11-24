#include <map>
#include <math.h>       /* pow */
#include <vector>
#include <algorithm>    // std::sort, std::stable_sort
#include <chrono>
#include <iostream>

#include <string>
#include <fstream>

#include <ctime>

#include <math.h> //square root

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
#define MAX_RGB_DIST 195075 //195075 = 255^2 * 3 - 0; since each pixel can be from 0 to 255
#define INT_MAX 2147483647

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
void batch_kmeans(const RGB_Image* img, const int k, RGB_Cluster* clusters, double* obj, double* iter_cnt)
{
	//step 1: initializing random centroids (aldready done in main)

	//Declare variables
	//reset counter to 1
	*iter_cnt = 1;
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
			double min_dist = MAX_RGB_DIST; //initiazize to be maximum distance possible between 2 pixels
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
		//printf("Iteration %d: SSE = %0.4f\n", *iter_cnt + 1, SSE);
		//printf("Iteration %d\n", *iter_cnt + 1); //testing

		//Step 5: If points stop changing their memberships, stop the loop
		if (num_pts_change == 0) {
			//save the objective then break
			*obj = SSE / img->size;
			break;
		}
		*iter_cnt += 1; //update counter
	}

	// Free the memory 
	free(centers_temp);
}

/* Color quantization using the batch neural gas algorithm */
void batch_neural_gas(const RGB_Image* img, const int k, RGB_Cluster* clusters, double* obj, double* iter_cnt) {
	//step 1: initializing random centroids (aldready done in main)

	//Declare variables
	double* LUT_h_func = (double*)malloc(k * sizeof(double)); //look up table for h_function
	//double* dist_array = (double*)malloc(k * sizeof(double)); //distances array
	vector<double> dist_array;
	double* weights = (double*)malloc(k * sizeof(double)); /*temporary weights*/
	RGB_Pixel* centers_temp = (RGB_Pixel*)malloc(k * sizeof(RGB_Pixel)); /*temporary centers of clusters in current iteration*/
	double lambda_initial = k / 2.0;
	double lambda;
	double current_cost = 0;
	double relative_cost = 1; //initialized with anything greater than CONV_THRESH(0.01)
	double previous_cost = 0;

	//reset counter
	*iter_cnt = 0;
	//double SSE = 0; //sum squared error, not objective functioin for bng

	//testing initial clusters' centers:
	cout << "\nInital clusters' centers are: " << endl;
	for (int i = 0; i < k; i++) {
		printf("Cluster %d's center is %.3f %.3f %.3f \n", i, clusters[i].center.red, clusters[i].center.green, clusters[i].center.blue);
	}

	/*step2: loop and stop until convergence threashold is reached or max iteration is reached*/
	while (*iter_cnt < MAX_ITERS && relative_cost > CONV_THRESH) //MAX_ITERS 
	{
		//iter_cnt < MAX_ITERS&& relative_cost > CONV_THRESH

		//First, calculate the decay constant for this iteration
		lambda = lambda_initial * pow((LAMBDA_FINAL / lambda_initial), (*iter_cnt / (double)MAX_ITERS)); //20.0  //MAX_ITERS
		//cout << "decay constant is: " << lambda << endl; //testing
		//cout << "*iter_cnt is : "<< *iter_cnt << endl;

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
		if (*iter_cnt == 0) {
			relative_cost = 1; //since 1st time running previous_cost is undefined, set relative_cost to 1 > CONV_THRESH (0.01)
		}
		else {
			relative_cost = (previous_cost - current_cost) / current_cost;
		}

		//Update previous_cost and iteration counter
		previous_cost = current_cost; //update previous cost
		*iter_cnt += 1; //update counter
	}
	*obj = current_cost / img->size;

	//testing initial clusters' centers:
	cout << "\nAfter bng updating, clusters' centers are: " << endl;
	for (int i = 0; i < k; i++) {
		printf("Cluster %d's center is %.3f %.3f %.3f \n", i, clusters[i].center.red, clusters[i].center.green, clusters[i].center.blue);
	}

	free(centers_temp);
	free(weights);
	free(LUT_h_func);
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

vector<string> tokenize_img_file(string s, string del = " ")
{
	vector<string> image_file;
	int start = 0;
	int end = s.find(del);
	while (end != -1) {
		image_file.push_back(s.substr(start, end - start));
		//cout << s.substr(start, end - start) << endl;
		start = end + del.size() + 1;
		end = s.find(del, start);
	}
	image_file.push_back(s.substr(start, end - start));
	//cout << s.substr(start, end - start);
	return image_file;
}

vector<int> tokenize_k_value(string s, string del = " ")
{
	vector<int> k_value;
	int start = 0;
	int end = s.find(del);
	while (end != -1) {
		k_value.push_back(stoi(s.substr(start, end - start))); //s.substr(start, end - start)
		//cout << s.substr(start, end - start) << endl;
		start = end + del.size() + 1;
		end = s.find(del, start);
	}
	k_value.push_back(stoi(s.substr(start, end - start)));
	//cout << s.substr(start, end - start);
	return k_value;
}


void calc_mean_stdev(double* objs, int num_runs,
	double* mean, double* stdev) {
	//a. Calcuate the mean
	double total_input = 0.0;
	for (int ir = 0; ir < num_runs; ir++)
	{
		total_input += objs[ir];
		//cout << "iters[ir]: " << iters[ir] << endl; //testing
	}
	*mean = total_input / num_runs;
	//b. Calcuate the standard deviation
	double objs_dev = 0.0; //squared deviation
	for (int ir = 0; ir < num_runs; ir++)
	{
		objs_dev += (objs[ir] - *mean) * (objs[ir] - *mean);
	}
	*stdev = sqrt(objs_dev / num_runs);
}

RGB_Cluster* copy_clusters(const RGB_Cluster* clusters, int k) {
	RGB_Cluster* clusters_copy = (RGB_Cluster*)malloc(k * sizeof(RGB_Cluster));
	for (int i = 0; i < k; i++) {
		clusters_copy[i].center.red = clusters[i].center.red;
		clusters_copy[i].center.green = clusters[i].center.green;
		clusters_copy[i].center.blue = clusters[i].center.blue;
		clusters_copy[i].size = 0;
	}
	return clusters_copy;
}

/*Save new image by assigning each pixel to the its belonging cluster's centroid*/
void map_pixels(RGB_Image* img, RGB_Cluster* clusters, int k) {
	//Iterate over each pixel in the img
	for (int i = 0; i < img->size; i++) { //loop to go over all pixels //img->size
		RGB_Pixel* pixel = &img->data[i];
		//cout << "\nrgb is: " << pixel->red << "," << pixel->green << "," << pixel->blue << endl; //testing
		double min_dist = MAX_RGB_DIST; //initiazize to be maximum distance possible between 2 pixels
		int min_index = -1;
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

// A recursive binary search function returns the index of closest item in array arr[l..r] compared to given x 
int binarySearch(const vector<double> arr, int l, int r, double x)
{
	if (arr.size() == 1) {
		return 0;
	}
	if (r >= l) {
		int mid = l + (r - l) / 2;

		// If the element is present at the middle itself
		if (arr[mid] == x) {
			return mid;
		}

		// If element is smaller than mid, then it can only be present in left subarray
		if (arr[mid] > x) {
			return binarySearch(arr, l, mid - 1, x);
		}

		// Else the element can only be present in right subarray
		return binarySearch(arr, mid + 1, r, x);
	}
	else {
		// We reach here when element is not present in array and we just want the closest element to x
		if (r == -1) {
			return 0;
		}
		if (l > arr.size() - 1) {
			return r;
		}
		double center = (arr[l] + arr[r]) / 2;
		if (x < center) {
			return r;
		}
		else {
			return l;
		}
	}
}

/* Euclidean distance
Implementation of fast mapping pixel using dimensionality reduction technique
proposed in "A fast and novel technique for color quantization using reduction of
color space dimensionality" paper
*/
//void fast_map_pixels(RGB_Image* img, RGB_Cluster* clusters, int k) {
//	//1. Compute the mean color vector x_mean of the palette colors.
//	RGB_Pixel temp;
//	temp.red = temp.green = temp.blue = 0.0;
//	RGB_Pixel x_mean; //mean color vector of the palette colors
//	x_mean.red = x_mean.green = x_mean.blue =  0.0;
//	for (int j = 0; j < k; j++) { //loop over each cluster
//		RGB_Cluster* cluster = &clusters[j];
//		temp.red += cluster->center.red;
//		temp.green += cluster->center.green;
//		temp.blue += cluster->center.blue;
//	}
//	x_mean.red = temp.red / k;
//	x_mean.green = temp.green / k;
//	x_mean.blue = temp.blue / k;
//	//2. For each palette color, compute the Euclidean distance between the palette color and the mean color vector.
//	//and 3. Find the color vector pmax with the largest distance relative to x_mean among all palette colors, and construct the line l for projecting.
//	double* pal_xmean_dists = (double*)malloc(k * sizeof(double)); /*array of distances between each palette color and the mean color vector*/
//	double max_dist = 0.0;
//	int max_index = -1;
//	for (int j = 0; j < k; j++) { 
//		RGB_Cluster* cluster = &clusters[j];
//		double delta_red = x_mean.red - cluster->center.red;
//		double delta_green = x_mean.green - cluster->center.green;
//		double delta_blue = x_mean.blue - cluster->center.blue;
//		double dist = sqrt(delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue);
//		pal_xmean_dists[j] = dist;
//		if (dist > max_dist) {
//			max_dist = dist;
//			max_index = j;
//		}
//	}
//
//	//printf("max_dist is: %0.4f\n", max_dist); //testing
//
//	//4. Project each palette color on the line l, and collect the projection values as a set S. 
//	//Sort S for the purpose of fast pixel mapping.
//	vector<double> S;
//	//double* S = (double*)malloc(k * sizeof(double));
//	for (int j = 0; j < k; j++) { //loop over each cluster
//		RGB_Cluster* cluster = &clusters[j];
//		double dot_prod = 0.0;
//		dot_prod = (cluster->center.red - x_mean.red) * (clusters[max_index].center.red - x_mean.red)
//			+ (cluster->center.green - x_mean.green) * (clusters[max_index].center.green - x_mean.green)
//			+ (cluster->center.blue - x_mean.blue) * (clusters[max_index].center.blue - x_mean.blue);
//		//dot_prod_sq = dot_prod_sq * dot_prod_sq;
//		double PS = max_dist - (dot_prod / max_dist); //nearest palette color p of x in terms of projection value
//		S.push_back(max_dist - (dot_prod / max_dist));
//	}
//
//	vector<int> sorted_index(S.size(), 0);
//	for (int i = 0; i != sorted_index.size(); i++) {
//		sorted_index[i] = i;
//	}
//	sort(sorted_index.begin(), sorted_index.end(),
//		[&](const int& a, const int& b) {
//			return (S[a] < S[b]);
//		}
//	);
//
//	sort(S.begin(), S.begin() + S.size());
//
//
//	//testing SS and sorted_index
//	//cout << "S: ";
//	//for (int i = 0; i < S.size(); i++) {
//	//	printf("%0.4f ", S[i]);
//	//	//cout << S[i] << " ";
//	//}
//	//cout << "\nsorted_index: ";
//	//for (int i = 0; i < sorted_index.size(); i++) {
//	//	cout << sorted_index[i] << " ";
//	//}
//
//	//5. For each pixel x do
//	double dot_prod = 0.0;
//	// save color vector pmax to limit access to the memory
//	RGB_Pixel pmax; // color vector with the largest distance relative to x_mean among all palette colors
//	pmax.red = clusters[max_index].center.red;
//	pmax.green = clusters[max_index].center.green;
//	pmax.blue = clusters[max_index].center.blue;
//	double threshhold = INT_MAX;
//	vector<double> SS;
//	vector<int> sorted_index_copy;
//	//double* SS = (double*)malloc(k * sizeof(double));
//	double PSp = 0.0; //nearest palette color p of x in terms of projection value
//	double PSx = 0.0; //projection value of x
//
//
//	for (int i = 0; i < img->size; i++) { //loop to go over all pixels //img->size //test 3 pixels first
//		RGB_Pixel* pixel = &img->data[i];
//		//5.1. Project x on l. Let PSx be the projection value of x.
//		dot_prod = (pixel->red - x_mean.red) * (pmax.red - x_mean.red)
//			+ (pixel->green - x_mean.green) * (pmax.green - x_mean.green)
//			+ (pixel->blue - x_mean.blue) * (pmax.blue - x_mean.blue);
//		//dot_prod = dot_prod * dot_prod;
//		PSx = max_dist - (dot_prod / max_dist);
//		dot_prod = 0.0; //reset 
//		//5.2. Set the value of a threshold to be a Maxint Integer.
//		threshhold = INT_MAX; //reset
//		//5.3. Copy the set S to a new set SS and same for sorted_index
//		SS = S;
//		sorted_index_copy = sorted_index;		
//		
//		//5.4. Do the following steps to find the optimal color 
//		//5.4.1. among the set SS, find the nearest palette color p of x in terms of projection value 
//		//(Called PSp) by the binary search method.
//		//5.4.2. If threashold > |PSp - PSx|, then threashold = D(x,p) ; SS <- SS - {p}
//		//5.4.3. Else Exit do-until loop.
//		//5.5. Until the set SS is empty. 
//		int index_cluster = -1;
//				
//		while (!SS.empty()) {
//			/*5.4.1. among the set SS, find the nearest palette color p of x in terms of projection value 
//				(Called PSp) by the binary search method.*/
//			int closest_index = binarySearch(SS, 0, SS.size() - 1, PSx);
//			//cout << "\nPSx is: " << PSx << endl;	//testing
//			//cout << "closest index is: " << closest_index; //testing
//			PSp = SS[closest_index];
//			//5.4.2. If threashold > |PSp - PSx|, then threashold = D(x,p) ; SS <- SS - {p}
//			if (threshhold > abs(PSp - PSx)) {
//				index_cluster = sorted_index_copy[closest_index];
//				//5.4.2.a) Calculate D(x,p): distance between the closest pallet and vector color pixel
//				double delta_red = clusters[index_cluster].center.red - pixel->red;
//				double delta_green = clusters[index_cluster].center.green - pixel->green;
//				double delta_blue = clusters[index_cluster].center.blue - pixel->blue;
//				double dist = sqrt(delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue);
//				threshhold = dist;
//				//5.4.2.b) SS <- SS - {p}
//				SS.erase(SS.begin() + closest_index);
//				sorted_index_copy.erase(sorted_index_copy.begin() + closest_index);
//			}
//			else {
//				break;
//			}
//		}
//		//5.6. Replace the pixel's color with the palette color obtained in the last step.
//		//save the pixel with the color of the centroid of its cluter
//		RGB_Cluster* nearest_cluster = &clusters[index_cluster];
//		pixel->red = nearest_cluster->center.red;
//		pixel->green = nearest_cluster->center.green;
//		pixel->blue = nearest_cluster->center.blue;
//	}
//	
//}


/* using Squared Euclidean distance
Implementation of fast mapping pixel using dimensionality reduction technique
proposed in "A fast and novel technique for color quantization using reduction of
color space dimensionality" paper
*/
void fast_map_pixels(RGB_Image* img, RGB_Cluster* clusters, int k) {
	//1. Compute the mean color vector x_mean of the palette colors.
	RGB_Pixel temp;
	temp.red = temp.green = temp.blue = 0.0;
	RGB_Pixel x_mean; //mean color vector of the palette colors
	x_mean.red = x_mean.green = x_mean.blue = 0.0;
	for (int j = 0; j < k; j++) { //loop over each cluster
		RGB_Cluster* cluster = &clusters[j];
		temp.red += cluster->center.red;
		temp.green += cluster->center.green;
		temp.blue += cluster->center.blue;
	}
	x_mean.red = temp.red / k;
	x_mean.green = temp.green / k;
	x_mean.blue = temp.blue / k;
	//2. For each palette color, compute the Euclidean distance between the palette color and the mean color vector.
	//and 3. Find the color vector pmax with the largest distance relative to x_mean among all palette colors, and construct the line l for projecting.
	double* pal_xmean_dists = (double*)malloc(k * sizeof(double)); /*array of distances between each palette color and the mean color vector*/
	double max_dist = 0.0;
	int max_index = -1;
	for (int j = 0; j < k; j++) {
		RGB_Cluster* cluster = &clusters[j];
		double delta_red = x_mean.red - cluster->center.red;
		double delta_green = x_mean.green - cluster->center.green;
		double delta_blue = x_mean.blue - cluster->center.blue;
		double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
		pal_xmean_dists[j] = dist;
		if (dist > max_dist) {
			max_dist = dist;
			max_index = j;
		}
	}

	//printf("max_dist is: %0.4f\n", max_dist); //testing

	//4. Project each palette color on the line l, and collect the projection values as a set S. 
	//Sort S for the purpose of fast pixel mapping.
	vector<double> S;
	//double* S = (double*)malloc(k * sizeof(double));
	for (int j = 0; j < k; j++) { //loop over each cluster
		RGB_Cluster* cluster = &clusters[j];
		double dot_prod_sq = 0.0;
		dot_prod_sq = (cluster->center.red - x_mean.red) * (clusters[max_index].center.red - x_mean.red)
			+ (cluster->center.green - x_mean.green) * (clusters[max_index].center.green - x_mean.green)
			+ (cluster->center.blue - x_mean.blue) * (clusters[max_index].center.blue - x_mean.blue);
		dot_prod_sq = dot_prod_sq * dot_prod_sq;
		double PS = max_dist - (dot_prod_sq / max_dist); //nearest palette color p of x in terms of projection value
		S.push_back(max_dist - (dot_prod_sq / max_dist));
	}

	vector<int> sorted_index(S.size(), 0);
	for (int i = 0; i != sorted_index.size(); i++) {
		sorted_index[i] = i;
	}
	sort(sorted_index.begin(), sorted_index.end(),
		[&](const int& a, const int& b) {
			return (S[a] < S[b]);
		}
	);

	sort(S.begin(), S.begin() + S.size());


	//testing SS and sorted_index
	//cout << "SS: ";
	//for (int i = 0; i < S.size(); i++) {
	//	printf("%0.4f ", S[i]);
	//	//cout << S[i] << " ";
	//}
	//cout << "\nsorted_index: ";
	//for (int i = 0; i < sorted_index.size(); i++) {
	//	cout << sorted_index[i] << " ";
	//}

	//5. For each pixel x do
	double dot_prod_sq = 0.0;
	// save color vector pmax to limit access to the memory
	RGB_Pixel pmax; // color vector with the largest distance relative to x_mean among all palette colors
	pmax.red = clusters[max_index].center.red;
	pmax.green = clusters[max_index].center.green;
	pmax.blue = clusters[max_index].center.blue;
	double threshhold = INT_MAX;
	vector<double> SS;
	vector<int> sorted_index_copy;
	//double* SS = (double*)malloc(k * sizeof(double));
	double PSp = 0.0; //nearest palette color p of x in terms of projection value
	double PSx = 0.0; //projection value of x


	for (int i = 0; i < img->size; i++) { //loop to go over all pixels //img->size //test 3 pixels first
		RGB_Pixel* pixel = &img->data[i];
		//5.1. Project x on l. Let PSx be the projection value of x.
		dot_prod_sq = (pixel->red - x_mean.red) * (pmax.red - x_mean.red)
			+ (pixel->green - x_mean.green) * (pmax.green - x_mean.green)
			+ (pixel->blue - x_mean.blue) * (pmax.blue - x_mean.blue);
		dot_prod_sq = dot_prod_sq * dot_prod_sq;
		PSx = max_dist - (dot_prod_sq / max_dist);
		dot_prod_sq = 0.0; //reset 
		//5.2. Set the value of a threshold to be a Maxint Integer.
		threshhold = INT_MAX; //reset
		//5.3. Copy the set S to a new set SS and same for sorted_index
		SS = S;
		sorted_index_copy = sorted_index;

		//5.4. Do the following steps to find the optimal color 
		//5.4.1. among the set SS, find the nearest palette color p of x in terms of projection value 
		//(Called PSp) by the binary search method.
		//5.4.2. If threashold > |PSp - PSx|, then threashold = D(x,p) ; SS <- SS - {p}
		//5.4.3. Else Exit do-until loop.
		//5.5. Until the set SS is empty. 
		int index_cluster = -1;

		while (!SS.empty()) {
			/*5.4.1. among the set SS, find the nearest palette color p of x in terms of projection value
				(Called PSp) by the binary search method.*/
			int closest_index = binarySearch(SS, 0, SS.size() - 1, PSx);
			//cout << "\nPSx is: " << PSx << endl;	//testing
			//cout << "closest index is: " << closest_index; //testing
			PSp = SS[closest_index];
			//5.4.2. If threashold > |PSp - PSx|, then threashold = D(x,p) ; SS <- SS - {p}
			if (threshhold > abs(PSp - PSx)) {
				index_cluster = sorted_index_copy[closest_index];
				//5.4.2.a) Calculate D(x,p): distance between the closest pallet and vector color pixel
				double delta_red = clusters[index_cluster].center.red - pixel->red;
				double delta_green = clusters[index_cluster].center.green - pixel->green;
				double delta_blue = clusters[index_cluster].center.blue - pixel->blue;
				double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;
				threshhold = dist;
				//5.4.2.b) SS <- SS - {p}
				SS.erase(SS.begin() + closest_index);
				sorted_index_copy.erase(sorted_index_copy.begin() + closest_index);
			}
			else {
				break;
			}
		}
		//5.6. Replace the pixel's color with the palette color obtained in the last step.
		//save the pixel with the color of the centroid of its cluter
		RGB_Cluster* nearest_cluster = &clusters[index_cluster];
		pixel->red = nearest_cluster->center.red;
		pixel->green = nearest_cluster->center.green;
		pixel->blue = nearest_cluster->center.blue;
	}

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
	clock_t begin = clock();
	cout << endl;
	/* Initialize centers */
	clusters = gen_rand_centers(img, k);
	double km_obj_value = 0.0;
	double km_iters_value = 0.0;
	double* km_obj = &km_obj_value;
	double* km_iters = &km_iters_value;
	//batch_kmeans(img, k, clusters);
	batch_kmeans(img, k, clusters, km_obj, km_iters);

	//calculate Mean Squared Error (MSE)
	/*double MSE = calcMSE(img, clusters, k);
	printf("Mean Squared Error (MSE) is: %f \n", MSE);*/

	//map each pixels to new clusters found
	//map_pixels(img, clusters, k);
	fast_map_pixels(img, clusters, k);

	//Make new image by writing to ppm file
	write_PPM(img, "outputting_img.ppm");

	cout << "\n------------------Finished running--------------------" << endl << endl;
	/* Stop Timer*/
	clock_t end = clock();
	double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
	printf("Time to run is: %.3f s", elapsed_secs);

	free(clusters);
	free_img(img);

	return 0;
}

