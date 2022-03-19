public class FizzBuzz {
  public static void main(String[] args) {
    for ( int i = 1; i <= 100; i++ ) {
      if ( i % 3 == 0 )
        System.out.print("Fizz");
      if ( i % 5 == 0 )
        System.out.print("Buzz");
      if ( i % 3 != 0 && i % 5 != 0 )
        System.out.println(i);
      else
        System.out.println("");
    }
  }
}
