#include <cuda.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <cublas.h>
#include <cuda_runtime.h>
#include "cublas_v2.h"
#include "omp.h"

//#include "C_codes_for_GPU.cuh"


#if CC12
  #define REAL float
#else
  #define REAL double
#endif


// ============================================================================
static void handleError(cudaError_t erro) {
    if(erro != cudaSuccess) {
        printf("%s\n", cudaGetErrorString(erro));
        exit(EXIT_FAILURE);
    }
}

// ============================================================================
// Driver for (t)asum_cublas

  extern "C" void call_asum_cublas(int n, REAL *vecx, int incx, REAL *res)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
			
#if CC12
	//printf("calling SASUM\n");		
	cublasSasum(handle,n,vecx_dev,incx,res);
#else	
	cublasDasum(handle,n,vecx_dev,incx,res);
#endif

	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)axpy_cublas 

  extern "C" void call_axpy_cublas(int n,REAL alpha, REAL *vecx, int incx, REAL *vecy,int incy)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev,*vecy_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	handleError(cudaMalloc((void **) &vecy_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_matrix,cudaMemcpyHostToDevice));	
			
#if CC12		
	cublasSaxpy(handle,n,&alpha,vecx_dev,incx,vecy_dev,incy);
#else		
	cublasDaxpy(handle,n,&alpha,vecx_dev,incx,vecy_dev,incy);
#endif

	handleError(cudaMemcpy(vecy,vecy_dev,size_matrix,cudaMemcpyDeviceToHost));

	handleError(cudaFree(vecy_dev));
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)copy_cublas 

extern "C" void call_copy_cublas(int n,REAL *vecx, int incx, REAL *vecy,int incy)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev,*vecy_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	handleError(cudaMalloc((void **) &vecy_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_matrix,cudaMemcpyHostToDevice));	
			
#if CC12		
	cublasScopy(handle,n,vecx_dev,incx,vecy_dev,incy);	
#else		
	cublasDcopy(handle,n,vecx_dev,incx,vecy_dev,incy);
#endif
	
	handleError(cudaMemcpy(vecy,vecy_dev,size_matrix,cudaMemcpyDeviceToHost));

	handleError(cudaFree(vecy_dev));
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)dot_cublas  

extern "C" void call_dot_cublas(int n,REAL *vecx, int incx, REAL *vecy,int incy, REAL *res)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev,*vecy_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	handleError(cudaMalloc((void **) &vecy_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_matrix,cudaMemcpyHostToDevice));	
			
#if CC12	
	cublasSdot(handle,n,vecx_dev,incx,vecy_dev,incy,res);
#else		
	cublasDdot(handle,n,vecx_dev,incx,vecy_dev,incy,res);
#endif

	handleError(cudaFree(vecy_dev));
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)gemm_cublas 

extern "C" void call_gemm_cublas(char tra, char trb, int m,int n, int k, REAL alpha, 
	  REAL *a, int lda, REAL *b, int ldb, REAL beta, REAL *c, int ldc)

{
	cublasHandle_t handle;
	cublasStatus_t status;
	cublasOperation_t transa,transb;
	status = cublasCreate(&handle);
	REAL *a_dev,*b_dev,*c_dev;
	size_t size_matrix_a,size_matrix_b,size_matrix_c;


	if(tra == 'N' && trb == 'N'){
		transa = CUBLAS_OP_N;
		transb = CUBLAS_OP_N;
		size_matrix_a = sizeof(REAL)*lda*k;
		size_matrix_b = sizeof(REAL)*ldb*n;
		size_matrix_c = sizeof(REAL)*ldc*n;
	}

	else if(tra == 'N' && trb == 'T'){
		transa = CUBLAS_OP_N;
		transb = CUBLAS_OP_T;
		size_matrix_a = sizeof(REAL)*lda*k;
		size_matrix_b = sizeof(REAL)*ldb*k;
		size_matrix_c = sizeof(REAL)*ldc*n;
	}
	else if(tra == 'T' && trb == 'N'){
		transa = CUBLAS_OP_T;
		transb = CUBLAS_OP_N;
		size_matrix_a = sizeof(REAL)*lda*m;
		size_matrix_b = sizeof(REAL)*ldb*n;
		size_matrix_c = sizeof(REAL)*ldc*n;
	}

	else if(tra == 'T' && trb == 'T'){
		transa = CUBLAS_OP_T;
		transb = CUBLAS_OP_T;
		size_matrix_a = sizeof(REAL)*lda*m;
		size_matrix_b = sizeof(REAL)*ldb*k;
		size_matrix_c = sizeof(REAL)*ldc*n;
	}

	handleError(cudaMalloc((void **) &a_dev, size_matrix_a));
	handleError(cudaMalloc((void **) &b_dev, size_matrix_b));
	handleError(cudaMalloc((void **) &c_dev, size_matrix_c));
	handleError(cudaMemcpyAsync(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpyAsync(b_dev,b,size_matrix_b,cudaMemcpyHostToDevice));
	handleError(cudaMemcpyAsync(c_dev,c,size_matrix_c,cudaMemcpyHostToDevice));

	cublasDgemm(handle, transa,transb,m,n,k,&alpha,a_dev,lda,b_dev,ldb,&beta,c_dev,ldc);


	handleError(cudaMemcpyAsync(c,c_dev,size_matrix_c,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(b_dev));
	handleError(cudaFree(c_dev));
	status = cublasDestroy(handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
			return;
	}
}

extern "C" void call_gemm_cublas_mgpu(char tra, char trb, int m,int n, int k, REAL alpha,
	  REAL *a, int lda, REAL *b, int ldb, REAL beta, REAL *c, int ldc)

{
	int ndevices;
	cudaGetDeviceCount(&ndevices);
	omp_set_num_threads(ndevices);
	cublasStatus_t status;


#pragma omp parallel shared(status)
	{
		int tid = omp_get_thread_num();
		int stride, rest, nstride, stride_out;

		cublasHandle_t handle;
		cublasOperation_t transa,transb;
		status = cublasCreate(&handle);
		REAL *a_dev,*b_dev,*c_dev;
		size_t size_matrix_a;

		if(tra == 'N' && trb == 'N'){
			transa = CUBLAS_OP_N;
			transb = CUBLAS_OP_N;
			size_matrix_a = sizeof(REAL)*lda*k;
		}

		else if(tra == 'N' && trb == 'T'){
			transa = CUBLAS_OP_N;
			transb = CUBLAS_OP_T;
			size_matrix_a = sizeof(REAL)*lda*k;
		}

		else if(tra == 'T' && trb == 'N'){
			transa = CUBLAS_OP_T;
			transb = CUBLAS_OP_N;
			size_matrix_a = sizeof(REAL)*lda*m;
		}

		else if(tra == 'T' && trb == 'T'){
			transa = CUBLAS_OP_T;
			transb = CUBLAS_OP_T;
			size_matrix_a = sizeof(REAL)*lda*m;
		}


		nstride = n / ndevices;
		rest = n % ndevices;
		stride = nstride * k;
		stride_out = nstride * m;

		handleError(cudaSetDevice(tid));

		handleError(cudaMalloc((void **) &a_dev, size_matrix_a));
		handleError(cudaMalloc((void **) &b_dev, sizeof(REAL)*stride  + (rest*k)*(tid == ndevices-1)));
		handleError(cudaMalloc((void **) &c_dev, sizeof(REAL)*stride  + (rest*m)*(tid == ndevices-1)));

		handleError(cudaMemcpyAsync(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
		handleError(cudaMemcpyAsync(b_dev,b+(stride*tid),sizeof(REAL)*(stride + (rest*k)*(tid == ndevices-1)),cudaMemcpyHostToDevice));
		handleError(cudaMemcpyAsync(c_dev,c+(stride*tid),sizeof(REAL)*(stride + (rest*m)*(tid == ndevices-1)),cudaMemcpyHostToDevice));

		cublasDgemm(handle, transa,transb,m,nstride + rest*(tid == ndevices-1),k,&alpha,a_dev,lda,b_dev,ldb,&beta,c_dev,ldc);

		handleError(cudaMemcpy(c + (stride_out*tid) ,c_dev,sizeof(REAL)*(stride_out + (rest*m)*(tid == ndevices-1)),cudaMemcpyDeviceToHost));
		handleError(cudaFree(a_dev));
		handleError(cudaFree(b_dev));
		handleError(cudaFree(c_dev));

		status = cublasDestroy(handle);

	}
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}


// ============================================================================
// Driver for (t)rot_cublas

  extern "C" void call_rot_cublas(int n,REAL *veci,int k,REAL *vecj,int l,REAL alpha,REAL beta)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_vec = sizeof(REAL)*n;	
	REAL *vecj_dev,*veci_dev;

	handleError(cudaMalloc((void **) &vecj_dev,size_vec));
	handleError(cudaMalloc((void **) &veci_dev,size_vec));
	
	handleError(cudaMemcpy(vecj_dev,vecj,size_vec,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(veci_dev,veci,size_vec,cudaMemcpyHostToDevice));	
			
#if CC12
	cublasSrot(handle,n,veci_dev,k,vecj_dev,l,&alpha,&beta);
#else
	cublasDrot(handle,n,veci_dev,k,vecj_dev,l,&alpha,&beta);
#endif

	handleError(cudaMemcpy(veci,veci_dev,size_vec,cudaMemcpyDeviceToHost));
	handleError(cudaMemcpy(vecj,vecj_dev,size_vec,cudaMemcpyDeviceToHost));

	handleError(cudaFree(veci_dev));
	handleError(cudaFree(vecj_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)gemv_cublas 

extern "C" void call_gemv_cublas(char tra, int m,int n, REAL alpha, REAL *a, int lda, 
             REAL *vecx, int incx, REAL beta, REAL *vecy, int incy)

{
	cublasHandle_t handle;
	cublasStatus_t status;
	cublasOperation_t transa;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		return;
	}

	size_t size_matrix_a, size_vec_x, size_vec_y;

    REAL *a_dev,*vecx_dev,*vecy_dev;

	if(tra == 'N' ){
		transa = CUBLAS_OP_N;

        size_matrix_a = sizeof(REAL)*lda*n;
		size_vec_x = sizeof(REAL)*n;
	    size_vec_y = sizeof(REAL)*m;
	}
	
	if(tra == 'T' ){
		transa = CUBLAS_OP_T;
        size_matrix_a = sizeof(REAL)*lda*m;
		size_vec_x = sizeof(REAL)*m;
	    size_vec_y = sizeof(REAL)*n;
	}

	handleError(cudaMalloc((void **) &a_dev,size_matrix_a));
	handleError(cudaMalloc((void **) &vecx_dev,size_vec_x));
	handleError(cudaMalloc((void **) &vecy_dev,size_vec_y));

	handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecx_dev,vecx,size_vec_x,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_vec_y,cudaMemcpyHostToDevice));
	
#if CC12
	cublasSgemv(handle, transa,m,n,&alpha,a_dev,lda,vecx_dev,incx,&beta,vecy_dev,incy);
#else
	cublasDgemv(handle, transa,m,n,&alpha,a_dev,lda,vecx_dev,incx,&beta,vecy_dev,incy);
#endif

	handleError(cudaMemcpy(vecy,vecy_dev,size_vec_y,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(vecx_dev));
	handleError(cudaFree(vecy_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}
  
// ============================================================================
// Driver for (t)ger_cublas 

  extern "C" void call_ger_cublas(int m,int n, REAL alpha, 
             REAL *vecx, int incx, REAL *vecy, int incy, REAL *a, int lda)

{
	cublasHandle_t handle;
	cublasStatus_t status;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		return;
	}

    size_t size_matrix_a = sizeof(REAL)*lda*n;
    size_t size_vec_x = sizeof(REAL)*m;
	size_t size_vec_y = sizeof(REAL)*n;
	REAL *a_dev,*vecx_dev,*vecy_dev;
	
	handleError(cudaMalloc((void **) &a_dev,size_matrix_a));
	handleError(cudaMalloc((void **) &vecx_dev,size_vec_x));
	handleError(cudaMalloc((void **) &vecy_dev,size_vec_y));

	handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecx_dev,vecx,size_vec_x,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_vec_y,cudaMemcpyHostToDevice));

#if CC12
	cublasSger(handle,m,n,&alpha,vecx_dev,incx,vecy_dev,incy,a_dev,lda);
#else
	cublasDger(handle,m,n,&alpha,vecx_dev,incx,vecy_dev,incy,a_dev,lda);
#endif

	handleError(cudaMemcpy(a,a_dev,size_matrix_a,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(vecx_dev));
	handleError(cudaFree(vecy_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)nrm2_cublas 
  
extern "C" void call_nrm2_cublas(int n,REAL *vecx, int incx, REAL *res)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
			
#if CC12	
	cublasSnrm2(handle,n,vecx_dev,incx,res);
#else		
	cublasDnrm2(handle,n,vecx_dev,incx,res);
#endif

	handleError(cudaMemcpy(vecx,vecx_dev,size_matrix,cudaMemcpyDeviceToHost));	
	
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)scal_cublas 

extern "C" void call_scal_cublas(int n,REAL alpha, REAL *vecx, int incx)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
			
#if CC12		
	cublasSscal(handle,n,&alpha, vecx_dev,incx);
#else		
	cublasDscal(handle,n,&alpha,vecx_dev,incx);
#endif

	handleError(cudaMemcpy(vecx,vecx_dev,size_matrix,cudaMemcpyDeviceToHost));	
	
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)swap_cublas

extern "C" void call_swap_cublas(int n,REAL *vecx, int incx, REAL *vecy,int incy)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev,*vecy_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	handleError(cudaMalloc((void **) &vecy_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecy_dev,vecy,size_matrix,cudaMemcpyHostToDevice));	
			
#if CC12		
	cublasSswap(handle,n,vecx_dev,incx,vecy_dev,incy);
#else		
	cublasDswap(handle,n,vecx_dev,incx,vecy_dev,incy);
#endif

	handleError(cudaMemcpy(vecx,vecx_dev,size_matrix,cudaMemcpyDeviceToHost));
	handleError(cudaMemcpy(vecy,vecy_dev,size_matrix,cudaMemcpyDeviceToHost));

	handleError(cudaFree(vecy_dev));
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)trmm_cublas 

extern "C" void call_trmm_cublas(char side, char uplo, char tra, char diag, int m, int n, 
	         REAL alpha, REAL *a, int lda, REAL *b, int ldb, REAL *c, int ldc)

{
	cublasHandle_t handle;
	cublasStatus_t status;
    cublasSideMode_t side_mode;
	cublasFillMode_t uplo_mode;
	cublasDiagType_t diag_mode;
	cublasOperation_t transa;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		return;
	}

	size_t size_matrix_a, size_matrix_b, size_matrix_c;

    REAL *a_dev,*b_dev,*c_dev;

	if (uplo == 'U' || uplo == 'u') {
		uplo_mode = CUBLAS_FILL_MODE_UPPER;
	}

	if (uplo == 'L' || uplo == 'l') {
		uplo_mode = CUBLAS_FILL_MODE_LOWER;
	}

    if (side == 'L' || side == 'l') {
		side_mode = CUBLAS_SIDE_LEFT;
	    size_matrix_a = sizeof(REAL)*lda*m;
        size_matrix_b = sizeof(REAL)*ldb*n;
	    size_matrix_c = sizeof(REAL)*ldc*n;
	}

	if (side == 'R' || side == 'r') {
		side_mode = CUBLAS_SIDE_RIGHT;
	    size_matrix_a = sizeof(REAL)*lda*n;
        size_matrix_b = sizeof(REAL)*ldb*n;
	    size_matrix_c = sizeof(REAL)*ldc*n;
	}

	if (diag == 'N' || diag == 'n') {
		diag_mode = CUBLAS_DIAG_NON_UNIT;
	}

	if (diag == 'U' || diag == 'u') {
		diag_mode = CUBLAS_DIAG_UNIT;
	}

	if(tra == 'N' || tra == 'n'){
		transa = CUBLAS_OP_N;		
	}

	if(tra == 'T' || tra == 't'){
		transa = CUBLAS_OP_T;		
	}

	//handleError( cudaMalloc((void **) &c_dev, size_norbs));
	handleError( cudaMalloc((void **) &a_dev, size_matrix_a));
	handleError( cudaMalloc((void **) &b_dev, size_matrix_b));
    handleError( cudaMalloc((void **) &c_dev, size_matrix_c));

	handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(b_dev,b,size_matrix_b,cudaMemcpyHostToDevice));
    handleError(cudaMemcpy(c_dev,a,size_matrix_c,cudaMemcpyHostToDevice));

#if CC12
	cublasStrmm(handle, side_mode, uplo_mode, transa,diag_mode, m,n,&alpha,a_dev,lda,
		        b_dev,ldb,c_dev,ldc);
#else
	cublasDtrmm(handle, side_mode, uplo_mode, transa,diag_mode, m,n,&alpha,a_dev,lda,
		        b_dev,ldb,c_dev,ldc);
#endif

	handleError(cudaMemcpy(c,c_dev,size_matrix_c,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(b_dev));
	handleError(cudaFree(c_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)trmv_cublas

extern "C" void call_trmv_cublas(char uplo, char tra, char diag, int n, REAL *a, int lda, 
             REAL *vecx, int incx)

{
	cublasHandle_t handle;
	cublasStatus_t status;
	cublasFillMode_t uplo_mode;
	cublasDiagType_t diag_mode;
	cublasOperation_t transa;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		return;
	}

	if (uplo == 'U' || uplo == 'u') {
		uplo_mode = CUBLAS_FILL_MODE_UPPER;
	}

	if (uplo == 'L' || uplo == 'l') {
		uplo_mode = CUBLAS_FILL_MODE_LOWER;
	}

	if (diag == 'N' || diag == 'n') {
		diag_mode = CUBLAS_DIAG_NON_UNIT;
	}

	if (diag == 'U' || diag == 'u') {
		diag_mode = CUBLAS_DIAG_UNIT;
	}

	if(tra == 'N' ){
		transa = CUBLAS_OP_N;
	}

	if(tra == 'T' ){
		transa = CUBLAS_OP_T;
	}    

	size_t size_matrix_a = sizeof(REAL)*lda*n;
    size_t size_vec_x = sizeof(REAL)*n;
	REAL *a_dev,*vecx_dev;

	handleError(cudaMalloc((void **) &a_dev,size_matrix_a));
	handleError(cudaMalloc((void **) &vecx_dev,size_vec_x));

	handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(vecx_dev,vecx,size_vec_x,cudaMemcpyHostToDevice));

#if CC12
	cublasStrmv(handle, uplo_mode, transa,diag_mode,n,a_dev,lda,vecx_dev,incx);
#else
	cublasDtrmv(handle, uplo_mode, transa,diag_mode,n,a_dev,lda,vecx_dev,incx);
#endif

	handleError(cudaMemcpy(vecx,vecx_dev,size_vec_x,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for (t)trsm_cublas 

extern "C" void call_trsm_cublas(char side, char uplo, char tra, char diag, int m, 
	         int n, REAL alpha, REAL *a, int lda, REAL *b, int ldb)

{
	cublasHandle_t handle;
	cublasStatus_t status;
    cublasSideMode_t side_mode;
	cublasFillMode_t uplo_mode;
	cublasDiagType_t diag_mode;
	cublasOperation_t transa;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		return;
	}

	size_t size_matrix_a,size_matrix_b;

    REAL *a_dev,*b_dev;

	if (uplo == 'U' || uplo == 'u') {
		uplo_mode = CUBLAS_FILL_MODE_UPPER;
	}

	if (uplo == 'L' || uplo == 'l') {
		uplo_mode = CUBLAS_FILL_MODE_LOWER;
	}

	if (diag == 'N' || diag == 'n') {
		diag_mode = CUBLAS_DIAG_NON_UNIT;
	}

	if (diag == 'U' || diag == 'u') {
		diag_mode = CUBLAS_DIAG_UNIT;
	}

	if(tra == 'N' ){
		transa = CUBLAS_OP_N;
	}

	if(tra == 'T' ){
		transa = CUBLAS_OP_T;
	}   

    if (side == 'L' || side == 'l') {
		side_mode = CUBLAS_SIDE_LEFT;
	    size_matrix_a = sizeof(REAL)*lda*m;
        size_matrix_b = sizeof(REAL)*ldb*n;
	}

	if (side == 'R' || side == 'r') {
		side_mode = CUBLAS_SIDE_RIGHT;
	    size_matrix_a = sizeof(REAL)*lda*n;
        size_matrix_b = sizeof(REAL)*ldb*n;
	}

	handleError(cudaMalloc((void **) &a_dev,size_matrix_a));
    handleError(cudaMalloc((void **) &b_dev,size_matrix_b));

	handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
	handleError(cudaMemcpy(b_dev,b,size_matrix_b,cudaMemcpyHostToDevice));

#if CC12
	cublasStrsm(handle, side_mode, uplo_mode, transa,diag_mode,m,n,&alpha,a_dev,lda,b_dev,ldb);
#else
	cublasDtrsm(handle, side_mode, uplo_mode, transa,diag_mode,m,n,&alpha,a_dev,lda,b_dev,ldb);
#endif

	handleError(cudaMemcpy(b,b_dev,size_matrix_b,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(b_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

// ============================================================================
// Driver for i(t)amax_cublas 

extern "C" void call_iamax_cublas(int n, REAL *vecx, int incx, int *res)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
			
#if CC12		
	cublasIsamax(handle,n,vecx_dev,incx,res);
#else		
	cublasIdamax(handle,n,vecx_dev,incx,res);
#endif
	
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}

  // ============================================================================
// Driver for i(t)amin_cublas 

extern "C" void call_iamin_cublas(int n, REAL *vecx, int incx, int *res)

{
	cublasHandle_t handle;
	cublasStatus_t status;

	status = cublasCreate(&handle);

	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed  \n");
		return;
	}

	size_t size_matrix = sizeof(REAL)*n;	
	REAL *vecx_dev;

	handleError(cudaMalloc((void **) &vecx_dev,size_matrix));
	
	handleError(cudaMemcpy(vecx_dev,vecx,size_matrix,cudaMemcpyHostToDevice));
			
#if CC12	
	cublasIsamin(handle,n,vecx_dev,incx,res);
#else		
	cublasIdamin(handle,n,vecx_dev,incx,res);
#endif
	
	handleError(cudaFree(vecx_dev));

	status = cublasDestroy(handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}


// ============================================================================
// Driver for (t)syrk_cublas 

extern "C" void call_syrk_cublas(char uplo, char tra, int n, int k, REAL alpha, 
			 REAL *a, int lda, REAL beta, REAL *c, int ldc)

{
	cublasHandle_t handle;
	cublasStatus_t status;
	cublasFillMode_t uplo_mode;
	cublasOperation_t transa;
	status = cublasCreate(&handle);

	if (status != CUBLAS_STATUS_SUCCESS){
		printf("Starting CUBLAS has failed \n");
		//return;
	}

	size_t size_matrix_a, size_matrix_c;

	REAL *a_dev,*c_dev;

	if (uplo == 'U' || uplo == 'u') {
		uplo_mode = CUBLAS_FILL_MODE_UPPER;
	}

	if (uplo == 'L' || uplo == 'l') {
		uplo_mode = CUBLAS_FILL_MODE_LOWER;
	}

	if(tra == 'N' || tra == 'n'){
		transa = CUBLAS_OP_N;		
	    size_matrix_a = sizeof(REAL)*lda*k;		
	    size_matrix_c = sizeof(REAL)*ldc*n;
	}

	if(tra == 'T' || tra == 't'){
		transa = CUBLAS_OP_T;		
	    size_matrix_a = sizeof(REAL)*lda*n;
	    size_matrix_c = sizeof(REAL)*ldc*n;
	}

	handleError(cudaMalloc((void **) &a_dev,size_matrix_a));
    handleError(cudaMalloc((void **) &c_dev,size_matrix_c));

	//handleError(cudaMemcpy(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
        //handleError(cudaMemcpy(c_dev,a,size_matrix_c,cudaMemcpyHostToDevice));
	handleError(cudaMemcpyAsync(a_dev,a,size_matrix_a,cudaMemcpyHostToDevice));
        handleError(cudaMemcpyAsync(c_dev,a,size_matrix_c,cudaMemcpyHostToDevice));

#if CC12
	cublasSsyrk(handle, uplo_mode, transa,n,k,&alpha,a_dev,lda,&beta,c_dev,ldc);
#else
	cublasDsyrk(handle, uplo_mode, transa,n,k,&alpha,a_dev,lda,&beta,c_dev,ldc);
#endif

	//handleError(cudaMemcpy(c,c_dev,size_matrix_c,cudaMemcpyDeviceToHost));
	handleError(cudaMemcpyAsync(c,c_dev,size_matrix_c,cudaMemcpyDeviceToHost));
	handleError(cudaFree(a_dev));
	handleError(cudaFree(c_dev));

	status = cublasDestroy(handle);
	if(status != CUBLAS_STATUS_SUCCESS){
		printf("Ending CUBLAS has failed \n");
		return;
	}
}






