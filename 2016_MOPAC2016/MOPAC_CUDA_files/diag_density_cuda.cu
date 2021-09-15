#include <cuda.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <cuda_runtime.h>

#if MAGMA
  #include <magma.h>
#endif

#include <omp.h>

#if CC12
  #define REAL float
#else
  #define REAL double
#endif

static bool init = false;
struct coeffs{
	double alpha, beta;
	int match;
	bool isGreater;
};

typedef struct coeffs coeff;

static void handleError(cudaError_t erro) {
    if(erro != cudaSuccess) {
        printf("%s\n", cudaGetErrorString(erro));
        exit(EXIT_FAILURE);
    }
}


__global__ void density_cuda(REAL *c, int norbs, REAL *p, int nn, int nl1, int nl2,
           int nu1, int nu2, REAL cst, REAL frac, REAL sign)
{
    int l,
        i,
        j,
        l_diag,
        k;

    REAL sum1,sum2;

    l = threadIdx.x + (blockIdx.x * blockDim.x) + 1;
    int stride = gridDim.x * blockDim.x;
    
    while(l <= nn)
    {
        i = (int)(sqrt((2*l) + 0.25) + 0.499);
        l_diag = (i*(i + 1))/2;

        /*if(l_diag < l) {
           j = l - l_diag;
           i = i + 1;
           }
        else {
           j = l - l_diag + i;
        } */
        
        i = i + 1*(l_diag < l);
        j = l - l_diag + i*(l_diag >= l);
        
        sum1 = 0.0;
        sum2 = 0.0;

        for(k = nl2; k <= nu2; k++)
        {
            sum2 += c[(k-1)*norbs + (i-1)] * c[(k-1)*norbs + (j-1)];
        }

        for(k = nl1; k <= nu1; k++)
        {
            sum1 += c[(k-1)*norbs + (i-1)] * c[(k-1)*norbs + (j-1)];
        }

        p[l-1] = (sum2*2.0 + sum1*frac)*sign;
        p[l-1] += (cst)*(i == j);

        /*p[l-1]=(REAL) j;*/

        l += stride;
    }
}

/* DENSIT driver for GPU */

    extern "C" void density_cuda_driver(REAL *c, int norbs, REAL *p, int nn, int nl1, int nl2,
           int nu1, int nu2, REAL cst, REAL frac, REAL sign, float *ttime, int nThreads, int nBlocks)

{
    //int nBlocks;
    REAL *c_dev,
          *p_dev;

    /*Time events in the GPU*/
    cudaEvent_t start,
                end;
    
    size_t size_nn,
           size_norbs;

    size_nn = sizeof(REAL)*nn;
    size_norbs = sizeof(REAL)*norbs*norbs;

    
    /*Starting events*/
    handleError(cudaEventCreate(&start));
    handleError(cudaEventCreate(&end));

    /*Starting to count time*/
    handleError(cudaEventRecord(start, 0));

    /*Allocating memory to GPU*/
    handleError(cudaMalloc((void **) &c_dev, size_norbs));
    handleError(cudaMalloc((void **) &p_dev, size_nn));

    /*Copying values to GPU*/
    handleError(cudaMemcpy( c_dev, c, size_norbs, cudaMemcpyHostToDevice));

    density_cuda <<< nBlocks, nThreads >>> (c_dev, norbs, p_dev, nn, nl1, nl2,
           nu1, nu2, cst, frac, sign);

    /*Taking back the results*/
    handleError(cudaMemcpy(p, p_dev, size_nn, cudaMemcpyDeviceToHost));


    /*calculating time spent in CUDA*/
    handleError(cudaEventRecord(end, 0));
    handleError(cudaEventSynchronize(end));
    handleError(cudaEventElapsedTime(ttime, start, end));

    /*deallocating memory in GPU*/
    handleError(cudaFree(c_dev));
    handleError(cudaFree(p_dev));

    /*destroying events in CUDA*/
    handleError(cudaEventDestroy(start));
    handleError(cudaEventDestroy(end));
}
    __global__ void computeOccs(coeffs *coeffs_occ,double *eig, double *fmo, int nocc, int n, int nvirt, double bigeps,double tiny){
    	double a,b,c,d,e,alpha, beta;
    	int stride,ij,tid;
    	int match, i;
    	int isGreater;
    	__shared__ int count;
    	if (threadIdx.x == 0){
    		count = 0;
    	}
    	__syncthreads();

    	tid = threadIdx.x+1;
    	stride = blockDim.x;
    	a = eig[blockIdx.x];
    	while(tid <= nvirt){
    		ij = (nvirt*(blockIdx.x) + (tid -1))+1;
    		b = eig[tid + nocc -1];
    		c = fmo[ij-1];
    		d = a - b;

    		e = copysign(sqrt(4.0*c*c + d*d),d);
    		alpha = sqrt(0.5*(1.0 + d/e));
    		beta = (-1.0)*copysign(sqrt(1.0 - alpha*alpha),c);


    		isGreater = (int)((abs(c) > tiny) && (abs(c/d) > bigeps));

    		match = tid;


    		if(isGreater){
				i = atomicAdd(&count, 1);
				coeffs_occ[((blockIdx.x)*nvirt + (i+1))-1].isGreater = isGreater;
				coeffs_occ[((blockIdx.x)*nvirt + (i+1))-1].match = match;
				coeffs_occ[((blockIdx.x)*nvirt + (i+1))-1].alpha = alpha;
				coeffs_occ[((blockIdx.x)*nvirt + (i+1))-1].beta  = beta;
    		}
    		__syncthreads();
    		tid += stride;
    	}
    }

    __global__ void computeVirt(coeffs *coeffs_virt,double *eig, double *fmo, int nocc, int n, int nvirt, double bigeps,double tiny){
        	double a,b,c,d,e,alpha, beta;
        	int stride,ij,tid;
        	int match, i;
        	int isGreater;
        	__shared__ int count;
        	if (threadIdx.x == 0){
        		count = 0;
        	}
        	__syncthreads();

        	tid = threadIdx.x+1;
        	stride = blockDim.x;
        	a = eig[blockIdx.x + nocc];
        	while(tid <= nocc){
        		ij = (nvirt*(tid -1) + (blockIdx.x))+1;
        		b = eig[tid -1];
        		c = fmo[ij-1];
        		d = a - b;

        		e = copysign(sqrt(4.0*c*c + d*d),d);
        		alpha = sqrt(0.5*(1.0 + d/e));
        		beta = (-1.0)*copysign(sqrt(1.0 - alpha*alpha),c);


        		isGreater = (int)((abs(c) > tiny) && (abs(c/d) > bigeps));

        		match = tid;


        		if(isGreater){
    				i = atomicAdd(&count, 1);
    				coeffs_virt[((blockIdx.x)*nocc + (i+1))-1].isGreater = isGreater;
    				coeffs_virt[((blockIdx.x)*nocc + (i+1))-1].match = match;
    				coeffs_virt[((blockIdx.x)*nocc + (i+1))-1].alpha = alpha;
    				coeffs_virt[((blockIdx.x)*nocc + (i+1))-1].beta  = beta;
        		}
        		__syncthreads();
        		tid += stride;
        	}
        }

    __global__ void rotateOccs(coeffs *coeffs_occ, double *vector, double *ca0,int nocc, int n, int nvirt){

    	int tid;
    	__shared__ coeffs coeff;
    	__shared__ double vecShared[256];
    	int count;
    	int nsweeps = (n/256) +1;
    	tid = threadIdx.x;

    	for(int i = 0; i < nsweeps; i++){
    		count = 0;
	    	vecShared[threadIdx.x] = 0;

			if (tid < n) vecShared[threadIdx.x] = vector[(blockIdx.x)*n + tid];

			while(coeffs_occ[blockIdx.x*(nvirt) + count].isGreater){


				coeff.alpha = coeffs_occ[blockIdx.x*(nvirt) + count].alpha;
				coeff.beta  = coeffs_occ[blockIdx.x*(nvirt) + count].beta;
				coeff.match = coeffs_occ[blockIdx.x*(nvirt) + count].match;

				if (tid < n) vecShared[threadIdx.x] = (coeff.alpha*vecShared[threadIdx.x]) + (coeff.beta * ca0[(coeff.match-1)*n + (tid)]);
				count++;
				__syncthreads();
			}

			if (tid < n) vector[(blockIdx.x)*n + tid] = vecShared[threadIdx.x];
			__syncthreads();
			tid += 256;
    	}
    }

    __global__ void rotateVirt(coeffs *coeffs_virt, double *vector, double *ci0,int nocc, int n, int nvirt){

       	int tid, bid;
        __shared__ coeffs coeff;
        __shared__ double vecShared[256];
        int count;
        int nsweeps = (n/256) +1;
        tid = threadIdx.x;
        bid = blockIdx.x + (nocc);

        for(int i = 0; i < nsweeps; i++){
        	count = 0;
    		__syncthreads();
            vecShared[threadIdx.x] = 0;

   			if (tid < n) vecShared[threadIdx.x] = vector[(bid)*n + tid];

   			while(coeffs_virt[blockIdx.x*(nocc) + count].isGreater){
   				coeff.alpha = coeffs_virt[blockIdx.x*(nocc) + count].alpha;
   				coeff.beta  = coeffs_virt[blockIdx.x*(nocc) + count].beta;
   				coeff.match = coeffs_virt[blockIdx.x*(nocc) + count].match;

   				if (tid < n) vecShared[threadIdx.x] = (coeff.alpha * vecShared[threadIdx.x]) - (coeff.beta * ci0[(coeff.match-1)*n + (tid)]);
   				count++;
   				__syncthreads();
    		}

    		if (tid < n) vector[(bid)*n + tid] = vecShared[threadIdx.x];
    		__syncthreads();
    		tid += 256;
        }
    }

    extern "C" void diag2GPU_Driver(double *fmo, double *eig, double *vector,double *ci0, double *ca0,
    		int nocc, int lumo, int n, double bigeps, double tiny){
    	double *eig_dev, *vector_dev, *fmo_dev, *ci0_dev, *ca0_dev;
    	coeffs *coeffs_occ,*coeffs_virt;
    	int nvirt = n-nocc;

    	size_t size_eig = sizeof(double)*n;
    	size_t size_n = sizeof(double)*n*n;
    	size_t size_ci0 = sizeof(double)*n*nocc;
    	size_t size_ca0 = sizeof(double)*n*nvirt;

    	handleError(cudaMalloc((void **)&coeffs_occ,sizeof(coeffs)*nocc*(nvirt)));
    	handleError(cudaMalloc((void **)&coeffs_virt,sizeof(coeffs)*(nvirt)*(nocc)));
    	handleError(cudaMalloc((void **)&eig_dev, sizeof(double)*n));
    	handleError(cudaMalloc((void **)&fmo_dev, sizeof(double)*n*n));
    	handleError(cudaMalloc((void **)&vector_dev, sizeof(double)*n*n));
    	handleError(cudaMalloc((void **)&ci0_dev, size_ci0));
    	handleError(cudaMalloc((void **)&ca0_dev, size_ca0));

    	handleError(cudaMemset(coeffs_occ,0,sizeof(coeffs)*nocc*nvirt));
    	handleError(cudaMemset(coeffs_virt,0,sizeof(coeffs)*nocc*nvirt));

    	handleError(cudaMemcpy(eig_dev, eig,size_eig,cudaMemcpyHostToDevice));
    	handleError(cudaMemcpy(fmo_dev,fmo,size_n,cudaMemcpyHostToDevice));
    	handleError(cudaMemcpy(vector_dev,vector,size_n,cudaMemcpyHostToDevice));
    	handleError(cudaMemcpy(ci0_dev, ci0,size_ci0,cudaMemcpyHostToDevice));
    	handleError(cudaMemcpy(ca0_dev, ca0,size_ca0,cudaMemcpyHostToDevice));


		computeOccs<<<nocc,256>>>(coeffs_occ,eig_dev,fmo_dev,nocc,n,nvirt,bigeps,tiny);
		computeVirt<<<nvirt,256>>>(coeffs_virt,eig_dev,fmo_dev,nocc,n,nvirt,bigeps,tiny);
		cudaDeviceSynchronize();

		rotateOccs<<<nocc,256>>>(coeffs_occ, vector_dev, ca0_dev,nocc, n, nvirt);
		rotateVirt<<<nvirt,256>>>(coeffs_virt, vector_dev, ci0_dev, nocc, n, nvirt);


    	handleError(cudaMemcpy(vector, vector_dev, sizeof(double)*n*n,cudaMemcpyDeviceToHost));


    	handleError(cudaFree(vector_dev));
    	handleError(cudaFree(fmo_dev));
    	handleError(cudaFree(eig_dev));
    	handleError(cudaFree(coeffs_occ));
    	handleError(cudaFree(coeffs_virt));
    	handleError(cudaFree(ci0_dev));
    	handleError(cudaFree(ca0_dev));

    	return;

    }

    extern "C" void diag2GPU_Driver_2gpu(double *fmo, double *eig, double *vector,double *ci0, double *ca0,
        		int nocc, int lumo, int n, double bigeps, double tiny){

    	omp_set_num_threads(2);

#pragma omp parallel
    	{
        	double *eig_dev, *vector_dev, *fmo_dev, *ci0_dev, *ca0_dev;
        	coeffs *coeffs_occ,*coeffs_virt;
        	int nvirt = n-nocc;
        	int tid;

        	size_t size_eig = sizeof(double)*n;
        	size_t size_n = sizeof(double)*n*n;
        	size_t size_ci0 = sizeof(double)*n*nocc;
        	size_t size_ca0 = sizeof(double)*n*nvirt;

			tid = omp_get_thread_num();
			cudaSetDevice(tid);

			if(tid == 0){
				handleError(cudaMalloc((void **)&coeffs_occ,sizeof(coeffs)*nocc*(nvirt)));
				handleError(cudaMalloc((void **)&eig_dev, sizeof(double)*n));
				handleError(cudaMalloc((void **)&fmo_dev, sizeof(double)*n*n));
				handleError(cudaMalloc((void **)&vector_dev, sizeof(double)*n*n));
				handleError(cudaMalloc((void **)&ca0_dev, size_ca0));


				handleError(cudaMemset(coeffs_occ,0,sizeof(coeffs)*nocc*nvirt));
				handleError(cudaMemcpy(eig_dev, eig,size_eig,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(fmo_dev,fmo,size_n,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(vector_dev,vector,size_n,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(ca0_dev, ca0,size_ca0,cudaMemcpyHostToDevice));
			}

			else{
				handleError(cudaMalloc((void **)&coeffs_virt,sizeof(coeffs)*(nvirt)*(nocc)));
				handleError(cudaMalloc((void **)&eig_dev, sizeof(double)*n));
				handleError(cudaMalloc((void **)&fmo_dev, sizeof(double)*n*n));
				handleError(cudaMalloc((void **)&vector_dev, sizeof(double)*n*n));
				handleError(cudaMalloc((void **)&ci0_dev, size_ci0));

				handleError(cudaMemset(coeffs_virt,0,sizeof(coeffs)*nocc*nvirt));
				handleError(cudaMemcpy(eig_dev, eig,size_eig,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(fmo_dev,fmo,size_n,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(vector_dev,vector,size_n,cudaMemcpyHostToDevice));
				handleError(cudaMemcpy(ci0_dev, ci0,size_ci0,cudaMemcpyHostToDevice));

			}

	#pragma omp barrier
			if(tid == 0){
				computeOccs<<<nocc,256>>>(coeffs_occ,eig_dev,fmo_dev,nocc,n,nvirt,bigeps,tiny);
				cudaDeviceSynchronize();
				rotateOccs<<<nocc,256>>>(coeffs_occ, vector_dev, ca0_dev,nocc, n, nvirt);

				handleError(cudaMemcpy(vector, vector_dev, sizeof(double)*n*nocc,cudaMemcpyDeviceToHost));
				handleError(cudaFree(vector_dev));
				handleError(cudaFree(fmo_dev));
				handleError(cudaFree(eig_dev));
				handleError(cudaFree(coeffs_occ));
				handleError(cudaFree(ca0_dev));
			}

			else{
				computeVirt<<<nvirt,256>>>(coeffs_virt,eig_dev,fmo_dev,nocc,n,nvirt,bigeps,tiny);
				cudaDeviceSynchronize();
				rotateVirt<<<nvirt,256>>>(coeffs_virt, vector_dev, ci0_dev, nocc, n, nvirt);

				handleError(cudaMemcpy(vector + (nocc*n) , vector_dev + (nocc*n), sizeof(double)*n*nvirt,cudaMemcpyDeviceToHost));
				handleError(cudaFree(vector_dev));
				handleError(cudaFree(fmo_dev));
				handleError(cudaFree(eig_dev));
				handleError(cudaFree(coeffs_virt));
				handleError(cudaFree(ci0_dev));
			}
	#pragma omp barrier
        }

   return;
}

#if MAGMA
    extern "C" void MagmaDsyevd_Driver1(int ngpus, char opt1, char opt2, int n, REAL *eigenvecs, int m,
        	REAL *eigvals, REAL *work_tmp, int lwork, int *iwork_tmp,int liwork, int *info){

        	REAL *eigenvecs_dev;
        	REAL *wa;
        	if (!init){
        		magma_init();
        		init = true;
        	}
        	if (ngpus == 1){
        		wa = (REAL *)malloc(sizeof(double)*n*n);
        		handleError(cudaMalloc((void **) &eigenvecs_dev, sizeof(double)*n*n));
        		handleError(cudaMemcpy(eigenvecs_dev, eigenvecs, sizeof(double)*n*n,cudaMemcpyHostToDevice));
        		magma_dsyevd_gpu(opt1,opt2 ,n, eigenvecs_dev,m,eigvals,wa,n,work_tmp,lwork, iwork_tmp,liwork,info);
        		free(wa);
        		cudaFree(eigenvecs_dev);
        	}
        	else {
        		magma_dsyevd_m(ngpus,opt1,opt2 ,n, eigenvecs,m,eigvals,work_tmp,lwork, iwork_tmp,liwork,info);
        	}
        	return;

        }

        extern "C" void MagmaDsyevd_Driver2(int ngpus,char opt1, char opt2, int n, REAL *eigenvecs, int m,
            REAL *eigvals, double *work, int lwork, int *iwork,int liwork, int *info){
        	REAL *eigenvecs_dev;
        	REAL *wa;

        	if (!init){
        		magma_init();
        		init = true;
        	}

        	if (ngpus == 1){
        		wa =  (REAL *)malloc(sizeof(double)*n*n);
        		handleError(cudaMalloc((void **) &eigenvecs_dev, sizeof(double)*n*n));
        		handleError(cudaMemcpy(eigenvecs_dev, eigenvecs, sizeof(double)*n*n,cudaMemcpyHostToDevice));
        		magma_dsyevd_gpu(opt1, opt2, n, eigenvecs_dev,m,eigvals,wa,n,work,lwork, iwork,liwork,info);
        		handleError(cudaMemcpy(eigenvecs,eigenvecs_dev,sizeof(double)*n*n,cudaMemcpyDeviceToHost));
        		free(wa);
        		cudaFree(eigenvecs_dev);
        	}
        	else {
        		magma_dsyevd_m(ngpus,opt1, opt2, n, eigenvecs,m,eigvals,work,lwork, iwork,liwork,info);
        	}
        	return;

        }

#endif

