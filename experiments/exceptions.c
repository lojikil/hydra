/* testing out execption handling at the C level, using setjmp/longjmp.
   need to test a few things:
   - top-level error handling
   - user-defined (SRFI-34) error handling within blocks, and falling back to the original
     block after passing through the user block
   - making it efficient.
 */
