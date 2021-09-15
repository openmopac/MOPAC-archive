#include "cuda.h"
#include <stdio.h>
#include <stdlib.h>

#if CC12
  #define REAL float
#else
  #define REAL double
#endif

static void handleError(cudaError_t erro) {
    if(erro != cudaSuccess) {
        printf("%s\n", cudaGetErrorString(erro));
        exit(EXIT_FAILURE);
    }
}

  __global__ void mamultc(REAL *a, REAL *b, REAL *c, REAL cst, int n, int size, int *ipoint) 

{
    int i, j, ii, jj, k1, k2, k3,l_diag;
    int tid = threadIdx.x + (blockIdx.x * blockDim.x)+1;
    int stride = gridDim.x * blockDim.x;
	REAL x;

    while(tid <= size) {

        REAL sum = 0.0;
		x = cst*c[tid-1];

        i = (int)(sqrt((2*tid) + 0.25) + 0.499);
        l_diag = (i*(i + 1))/2;

        /*if(l_diag < tid) {
           j = tid - l_diag;
           i = i + 1;
           }
        else {
           j = tid - l_diag + i;
        } */
        
        i = i + 1*(l_diag < tid);
        j = tid - l_diag + i*(l_diag >= tid);

       /* j = tid - (((i-1)*i)/2);*/

        ii = ipoint[i-1];
        jj = ipoint[j-1];

        for (k1 = 1; k1 <= j; k1++) {
            sum += a[ii+k1-1]*b[jj+k1-1];
        }

        for (k2 = j+1; k2 <= i; k2++) {
            int kk = ipoint[k2-1];
            sum += a[ii+k2-1]*b[kk+j-1];
        }

        for (k3 = i+1; k3 <= n; k3++) {
            int kk = ipoint[k3-1];
            sum += a[kk+i-1]*b[kk+j-1];
        }

        c[tid-1] = sum + x;
        tid += stride;
    }
}

  __global__ void mamult2(REAL *a, REAL *b, REAL *c, REAL cst, int n)

{
    int bid,
            k;

    bid = blockIdx.x + 1;
    REAL x;

    while(bid <= n) {
        int ii = ((bid-1)*bid)/2;
        int tid = threadIdx.x + 1;

        while(tid <= bid) {
            int jj = ((tid-1)*tid)/2;
            REAL sum = 0;
            
			int l;

            for (k = 1; k <= tid; k++) {
                sum += a[ii+k-1]*b[jj+k-1];
            }

            for (k = tid+1; k <= bid; k++) {
                sum += a[ii+k-1]*b[(((k-1)*k)/2) + tid-1];
            }

            for (k = bid+1; k <= n; k++) {
                int kk = (k*(k-1))/2;
                sum += a[kk+bid-1]*b[kk+tid-1];
            }

            l = tid + ((bid-1)*bid)/2;
			
			x = cst*c[l-1];
            
			c[l-1] = sum + x;

            tid += blockDim.x;
        }

        bid += gridDim.x;
    }
}

  __global__ void mamult3(REAL *a, REAL *b, REAL *c, int n1, int n2, int n) 

{
    int bid,
            k;

    bid = blockIdx.x + n1;

    while(bid <= n2) {
        int ii = ((bid-1)*bid)/2;
        int tid = threadIdx.x + 1;

        while(tid <= bid) {
            int jj = ((tid-1)*tid)/2;
            REAL sum = 0;
            int l;

            for (k = 1; k <= tid; k++) {
                sum += a[ii+k-1]*b[jj+k-1];
            }

            for (k = tid+1; k <= bid; k++) {
                int kk = (((k-1)*k)/2);
                sum += a[ii+k-1]*b[kk + tid-1];
            }

            for (k = bid+1; k <= n; k++) {
                int kk = (k*(k - 1))/2;
                sum += a[kk + bid - 1]*b[kk + tid - 1];
            }

            l = tid + ((bid-1)*bid)/2;
            c[l-1] = sum;

            tid += blockDim.x;
        }

        bid += gridDim.x;
    }
}

  extern "C" void mamult_driver(REAL *a, REAL *b, REAL *c, int n, int nn,
        int *ipoint, REAL cst, int gridx, int blockx, float *tempo, int job) 
{

    REAL *a_dev,*b_dev,*c_dev;

    int *ipoint_dev;

    size_t size_nn,
            size_n;

    /*Time events for GPU*/
    cudaEvent_t inicio,
        fim;

    int i;

    size_nn = sizeof(REAL)*nn;

    size_n = sizeof(int)*n;

    /*Starting events*/
    handleError( cudaEventCreate(&inicio));
    handleError( cudaEventCreate(&fim));

    /*Starting to count time*/
    handleError( cudaEventRecord(inicio, 0));

    /*Allocating memory to GPU*/
    handleError( cudaMalloc((void **) &a_dev, size_nn));
    handleError( cudaMalloc((void **) &b_dev, size_nn));
    handleError( cudaMalloc((void **) &c_dev, size_nn));
    handleError( cudaMalloc((void **) &ipoint_dev, size_n));

    /*Copying values to GPU*/
    handleError( cudaMemcpy( a_dev, a, size_nn, cudaMemcpyHostToDevice));
    handleError( cudaMemcpy( b_dev, b, size_nn, cudaMemcpyHostToDevice));
    handleError( cudaMemset( c_dev, 0, size_nn));
    handleError( cudaMemcpy( ipoint_dev, ipoint, size_n, cudaMemcpyHostToDevice));

    /*Choosing the kernel call*/
    switch (job)  {
        case 0:
            mamultc <<< gridx, blockx >>> (a_dev, b_dev, c_dev, cst, n, nn, ipoint_dev);
            break;
        case 1:
            mamult2 <<< gridx, blockx >>> (a_dev, b_dev, c_dev, cst, n);
            break;
        case 2:
            for (i = 1; i+10 <= n; i += 10) {
                printf("i=%d\n", i);
                mamult3 <<< gridx, blockx >>> (a_dev, b_dev, c_dev, i, i+9, n);
            }
            printf("i=%d, n=%d\n", i, n);
            mamult3 <<< gridx, blockx >>> (a_dev, b_dev, c_dev, i, n, n);

            break;
        default:
            printf("\n(CUDA) Undefined Job\n");
    }   

    /*Taking back the results*/
    handleError( cudaMemcpy(c, c_dev, size_nn, cudaMemcpyDeviceToHost));

    /*calculating time spent in CUDA*/
    handleError( cudaEventRecord(fim, 0));
    handleError( cudaEventSynchronize(fim));
    handleError( cudaEventElapsedTime(tempo, inicio, fim));

    /*deallocating memory in GPU*/
    handleError( cudaFree(a_dev));
    handleError( cudaFree(b_dev));
    handleError( cudaFree(c_dev));
    handleError( cudaFree(ipoint_dev));

    /*destroying events in CUDA*/
    handleError( cudaEventDestroy(inicio));
    handleError( cudaEventDestroy(fim));
}

