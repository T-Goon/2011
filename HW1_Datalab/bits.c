/*
 * CS:APP Data Lab
 *
 * Timothy Goon
 * twgoon@wpi.edu
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function.
     The max operator count is checked by dlc. Note that '=' is not
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */

/*
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void) {
  /*
  Constructs a 8-bit int with the odd numbered bits set to 1.
  Then uses logical left-shifts to put 4 of these ints next to each other,
  constructing a full word with all odd bits set to 1.
  */
  int aa = 0xaa;
  int answer = aa;
  answer = (answer << 8) + aa;
  answer = (answer << 8) + aa;
  answer = (answer << 8) + aa;
  return answer;
}
/*
 * isTmin - returns 1 if x is the minimum, two's complement number,
 *     and 0 otherwise
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmin(int x) {
  /*
  Tmin is the only number, when added to itself, will overflow to 0.
  Using '!', 1 will be returned.
  "^ !x" is added for the x=0 case. Without it x=0 would return 1.
  */
  return (!(x+x) ^ !(x));
}
/*
 * bitXor - x^y using only ~ and &
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  /*
  'a' - the bits only in x
  'b' - the bits only in y
  ~(~a & ~b) - used to combine bits in 'a' and 'b'
  */
  int a = (x & ~y);
  int b = (y & ~x);
  return  ~(~a & ~b);
}
/*
 * conditional - same as x ? y : z
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  // Figures out if x is true or false.
  // if x is true the first will be true and second will be false
  // if x is false then second will be true and first will be false
  int first = !!x;
  int second = !x;

  // will fill the entire int with ones if the first bit is a one
  // will will the entire int with zeros of the first bit is a zero
  first = ~first + 1;
  second = ~second + 1;

  // uses first and second to set either y or z to 0 depending on if x is true or false
  y = y & first;
  z = z & second;

  // when added answer will be either y or z, since one of them has been set to 0
  return y + z;
}
/*
 * greatestBitPos - return a mask that marks the position of the
 *               most significant 1 bit. If x == 0, return 0
 *   Example: greatestBitPos(96) = 0x40
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 *   Rating: 4
 */
int greatestBitPos(int x) {
  int isNeg, doShift, isZero, mask;
  // Fills in all bits right of the most significant bit with ones.
  x = x | (x >> 1);
  x = x | (x >> 2);
  x = x | (x >> 4);
  x = x | (x >> 8);
  x = x | (x >> 16);

  // Check if the sign bit is 1 or not.
  isNeg = 1 << 31;
  isNeg = isNeg & x;

  // If the sign bit is 1, overflow it and set it to 0.
  x = x + isNeg;

  // If the sign bit is overflowed and set to 0,
  // we don't want to do the upcoming right shift. (shift by 0)
  doShift = !isNeg;

  // Use ! on x to check if x is zero or not
  isZero = 1;
  isZero = isZero & !(x);

  /*
  If the sign bit was not detected as 1, shift x over by one bit
  then add 1 to the number. The result being the most significant bit of the
  input.

  If the sign bit was detected, don't shift and add 1 to the number.
  The result being the sign bit set to 1 and the rest of the bits 0.
  */
  mask = (x >> doShift) + 1;

  /*
  If x is zero then the above code makes mask = 1 and isZero = 1.
  Shift mask one to the right if it is 0 to make mask 0.
  */
  mask = mask >> isZero;

  return mask;
}
/*
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {
  /*
  Use the fact the a right shift of x by in will result in x/(2^n).
  To avoid the rounding problem with negative numbers, x is change back to a
  positive one before calculating the answer.
  */
  int isNeg, doAdd;

  /*
  Use isNeg and doAdd to change x back to a positive number if it was passed in
  as negative.
  */
  // Check if x is negative or not
  isNeg = 1 << 31;
  // isNeg will stay the same if x is negative and become 0 if x is not negative.
  isNeg = isNeg & x;
  // Will make isNeg either all 1s or all 0s.
  isNeg = isNeg >> 31;

  /*
  Will be
  If x is Tmin doAdd will be 0.
  In any other case doAdd will be 1 if isNeg is all 1s and 0 if isNeg is all 0s.
  */
  doAdd = (!!(x + x)) & isNeg;

  /*
  Make x a positive number.
  Will only flip the bits if isNeg is all 1s.
  Will only add by 1 if isNeg is all 1s and x is not Tmin.
  */
  x = (x ^ isNeg) + doAdd;

  // Do the division with a right shift.
  x = x >> n;

  // If x was originally negative change it back to a negative number.
  x = (x ^ isNeg) + doAdd;
  return x;
}
/*
 * isNonNegative - return 1 if x >= 0, return 0 otherwise
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
  /*
  Tmin & x, x being any number, will give Tmin if
  x is negative and 0 if x if positive.
  Using ! on that result will give 0 when the result is Tmin and 1 if
  the result is 0.
  */
  int isNeg, answer;

  isNeg = 1 << 31;
  isNeg = isNeg & x;

  answer = !(isNeg);

  return answer;
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x60000000) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
  /*
  Uses the properties of arithmetic right shifting to detect if there is an
  overflow when adding and saturates the answer if neccessary.
  */
  int Tmin, isTmin, answer, isDif, shift;
  // Uses Tmin to detect if x is Tmin.
  Tmin = 1 << 31;
  isTmin = !(Tmin + x);
  // isTmin = Tmin if x = Tmin, otherwise isTmin = 0.
  isTmin = isTmin << 31;

  // Use addition to multiply x by 2
  answer = x + x;

  // is all 1s if the signs of x and answer(x+x) are different
  // detects overflow when adding
  isDif = (x >> 31) ^ (answer >> 31);

  // shift is 31 only if an overflow is detected, otherwise 0
  shift = isDif & ~(Tmin >> 26);

  // right shift answer by 31 only if there is overflow
  // saturate answer if there is overflow and make it all 1s
  answer = answer >> shift;

  // Add Tmin to answer only if there is overflow
  // Changes answer from 0xFFFFFFFF to 0x7FFFFFFF
  Tmin = Tmin & isDif;
  answer = answer + Tmin;

  return answer;
}
/*
 * isLess - if x < y  then return 1, else return 0
 *   Example: isLess(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLess(int x, int y) {
  int Tmin, negY, diff, isDiffNeg, doAdd,
  sign1, sign2, diffSign, overflow, signSame,
  xySame;
  /*
  Uses the fact that x - y will always be negative or 0 if x is less than y to find
  out if x is less than y.
  */
  Tmin = (1 << 31);

  // Change y to -y
  doAdd = !!(Tmin + y);
  negY = (~y) + doAdd;

  // Calculate x - y
  diff = x + negY;

  /*
  Checks if x - y overflowed.
  To get an overflow x and y need to have the same sign and the sign
  of the difference must be different than the sign of x and y.
  */
  sign1 = Tmin & negY;
  sign2 = Tmin & x;
  signSame = ~(sign1 ^ sign2);
  diffSign = Tmin & diff;
  overflow = signSame & (sign1 ^ diffSign);

  // If x and y are the same number x cannot be less than y.
  xySame = !!(x ^ y);
  /*
  Is true if:
    -> x-y is negative and there is no overflow OR
      x-y is positive and there is overflow
    -> x != y
  */
  isDiffNeg = xySame & (!!((Tmin & diff) ^ overflow));

  return isDiffNeg;
}
/*
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
  return 2;
}
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
  return 2;
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {
  return 2;
}
/*
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
 return 2;
}
/*
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  return 2;
}
/*
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
  return 2;
}
