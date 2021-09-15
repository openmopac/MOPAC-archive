/**
 * @file gpu_info.cu
 * @author Carlos Peixoto M Junior and Julio Daniel Carvalho Maia
 * @date 11/14/2013
 */
 
/*****************************************************************************/
/*                            INCLUDES                                       */
/*****************************************************************************/
#include <cuda.h>
#include <stdio.h>
#include <stdlib.h>

/**
 *
 */
extern "C" void getGPUInfo(bool *hasGpu, bool *hasDouble, int *nDevices, char *name, int *name_size, 
						   size_t *totalMem, int *clockRate, int *major, int *minor)
{
    int n;
    cudaError_t error = cudaGetDeviceCount(&n);
    
    if(error == cudaErrorNoDevice)
    {
        *hasGpu    = false;
        //*hasDouble = false;
        *nDevices  = 0;
    }
    
    else if(error == cudaErrorInsufficientDriver)
    {
        *hasGpu    = false;
        //*hasDouble = false;
        *nDevices  = 0;
    }
    else
    {
        *hasGpu    = true;
        *nDevices  = n;
        cudaDeviceProp prop;
        

	for (int i = 0; i < n; i++){

		if(cudaGetDeviceProperties(&prop, i) != cudaErrorInvalidDevice)
		{
		    if(prop.major >= 2)
		        hasDouble[i] = true;
		    else
		        hasDouble[i] = false;
		}
						
		strcpy(name + (i*256), prop.name);
		name_size[i] = (int)strlen(name + (i*256));
		totalMem[i] = prop.totalGlobalMem;
		clockRate[i] = prop.clockRate;
		minor[i] = prop.minor;
		major[i] = prop.major;
    
	}
}
}

extern "C" void setDevice(int idevice, bool *stat){
    cudaError_t erro;
    erro = cudaSetDevice(idevice);
    if (erro == cudaSuccess) *stat = true;
    else *stat = false;
}

