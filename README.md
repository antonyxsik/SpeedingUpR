# SpeedingUpR
(2022 most recent) A very simple guide to speeding up R on personal Windows devices. 

Revisions: 
* Revision 1: Rev1 is now uploaded, and provides an easy way for Window's users to significantly speed up matrix operations on their personal device via the use of parallelization (using multiple threads) of their CPU. The guide contains both background information, instructions, and timing results on my personal device, often achieving speedups between 10 - 100x. 

Note: those uninterested in anything additional and just want to speed up R should go to the "An Easy Speedup for Windows Users" section and follow the instructions there! The code attached is timing code that you can use on your personal device!

* Revision 2: Rev 2 will be future work with outsourcing the matrix operations to the GPU. For users with advanced graphics cards (an easy example is most people with a gaming laptop), this will provide an even faster speedup than the CPU strategy. 

NOTE: Window's users who just want to speed up their R on their personal devices: skip straight to the "An Easy Speedup for Windows Users" section. The instructions there should be easy to follow, the background information will be helpful, and the timing results are just pleasing to look at!
