# Cobol Course for MVS 3.8 on Hercules
## Book 'Elementary Structured Cobol A Step by Step Approach'
I am very interested in mainframes and have discovered the wonderful [Hercules](http://www.hercules-390.org/) emulator and [MVS 3.8](https://www.jaymoseley.com/hercules/). I have a special interest in COBOL and I am always looking for information on how people programmed back then.

One of the very old books for this is **Elementary Structured Cobol, A Step By Step Approach** from 1977. I have made the examples available here for anyone interested, including the required JCL code.
It may not be very high level, but maybe it will help some beginners like me to make the first steps in MVS 3.8.

### Note: MVS OS/VS Compiler 1.2.4
The MVT COBOL compiler that comes with the MVS 3.8 distributions is freely available, but it is really very very old. Recently, a somewhat newer version of OS/VS COBOL 1.2.4 appeared in a relevant discord channel. I installed this compiler and compiled the examples with it. However, the only change that is necessary to be able to compile the examples with MVT COBOL should be the change from COB2UCG to COBUCG in the JCL code. 
