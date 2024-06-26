# Cobol Course for MVS 3.8 on Hercules
## Book 'Elementary Structured Cobol A Step by Step Approach'
I am very interested in mainframes and have discovered the wonderful [Hercules](http://www.hercules-390.org/) emulator and [MVS 3.8](https://www.jaymoseley.com/hercules/). I have a special interest in COBOL and I am always looking for information on how people programmed back then.

One of the very old books for this is **Elementary Structured Cobol, A Step By Step Approach** from 1977. I have made the examples available here for anyone interested, including the required JCL code.
It may not be very high level, but maybe it will help some beginners like me to make the first steps in MVS 3.8.

### Editor and Submitting JCL
I'm typing these programs in Visual Code with the IBM Z Open Editor Extension. Afterwards I'm submitting the JCL file which contains the COBOL code and also the data input cards. For that I use the [submit.sh](https://www.jaymoseley.com/hercules/compiling/how_to.htm#topic1) from Jay Moseley.

### Note: MVS OS/VS Compiler 1.2.4
The MVT COBOL compiler that comes with the MVS 3.8 distributions is freely available, but it is really very very old. Recently, a somewhat newer version of OS/VS COBOL 1.2.4 appeared in a relevant discord channel. I installed this compiler and compiled the examples with it. I haven't testet with the MVT COBOL from MVS 3.8 yet.
