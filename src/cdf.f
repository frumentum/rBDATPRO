c*********************************************************************72
      subroutine normal_cdf ( x, a, b, cdf )

c*********************************************************************72
c
cc NORMAL_CDF evaluates the Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision y

      y = ( x - a ) / b

      call normal_01_cdf ( y, cdf )

      return
      end

c*********************************************************************72	  
	  subroutine normal_01_cdf ( x, cdf )

c*********************************************************************72
c
cc NORMAL_01_CDF evaluates the Normal 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    AG Adams,
c    Algorithm 39,
c    Areas Under the Normal Curve,
c    Computer Journal,
c    Volume 12, pages 197-198, 1969.
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a1
      parameter ( a1 = 0.398942280444D+00 )
      double precision a2
      parameter ( a2 = 0.399903438504D+00 )
      double precision, parameter :: a3 = 5.75885480458D+00
      double precision, parameter :: a4 = 29.8213557808D+00
      double precision, parameter :: a5 = 2.62433121679D+00
      double precision, parameter :: a6 = 48.6959930692D+00
      double precision, parameter :: a7 = 5.92885724438D+00
      double precision, parameter :: b0 = 0.398942280385D+00
      double precision, parameter :: b1 = 3.8052D-08
      double precision, parameter :: b2 = 1.00000615302D+00
      double precision, parameter :: b3 = 3.98064794D-04
      double precision, parameter :: b4 = 1.98615381364D+00
      double precision, parameter :: b5 = 0.151679116635D+00
      double precision, parameter :: b6 = 5.29330324926D+00
      double precision, parameter :: b7 = 4.8385912808D+00
      double precision, parameter :: b8 = 15.1508972451D+00
      double precision, parameter :: b9 = 0.742380924027D+00
      double precision, parameter :: b10 = 30.789933034D+00
      double precision b11
      parameter ( b11 = 3.99019417011D+00 )
      double precision cdf
      double precision q
      double precision x
      double precision y
c
c  |X| .le. 1.28.
c
      if ( abs ( x ) .le. 1.28D+00 ) then

        y = 0.5D+00 * x * x

        q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + 
     &a5       + a6 / ( y + a7 ) ) ) )
c
c  1.28 .lt. |X| .le. 12.7
c
      else if ( abs ( x ) .le. 12.7D+00 ) then

        y = 0.5D+00 * x * x

        q = exp ( - y ) * b0 / ( abs ( x ) - b1       + b2 / ( abs ( x )
     & + b3       + b4 / ( abs ( x ) - b5       + b6 / ( abs ( x ) + b7 
     &      - b8 / ( abs ( x ) + b9       + b10 / ( abs ( x ) + b11 ) ) 
     &) ) ) )
c
c  12.7 .lt. |X|
c
      else

        q = 0.0D+00

      end if
c
c  Take account of negative X.
c
      if ( x .lt. 0.0D+00 ) then
        cdf = q
      else
        cdf = 1.0D+00 - q
      end if

      return
      end
	  
c*********************************************************************72	  	  
	  subroutine normal_cdf_inv ( cdf, a, b, x )

c*********************************************************************72
c
cc NORMAL_CDF_INV inverts the Normal CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision CDF, the value of the CDF.
c    0.0D+00 .le. CDF .le. 1.0.
c
c    Input, double precision A, B, the parameters of the PDF.
c    0.0D+00 .lt. B.
c
c    Output, double precision X, the corresponding argument.
c
      implicit none

      double precision a
      double precision b
      double precision cdf
      double precision x
      double precision x2

      if ( cdf .lt. 0.0D+00 .or. 1.0D+00 .lt. cdf ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NORMAL_CDF_INV - Fatal error!'
        write ( *, '(a)' ) '  CDF .lt. 0 or 1 .lt. CDF.'
        stop
      end if

      call normal_01_cdf_inv ( cdf, x2 )

      x = a + b * x2

      return
      end

	  
c*********************************************************************72	  
	  subroutine normal_01_cdf_inv ( p, x )

c*********************************************************************72
c
cc NORMAL_01_CDF_INV inverts the standard normal CDF.
c
c  Discussion:
c
c    The result is accurate to about 1 part in 10**16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 August 2013
c
c  Author:
c
c    Original FORTRAN77 version by Michael Wichura.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Michael Wichura,
c    Algorithm AS241:
c    The Percentage Points of the Normal Distribution,
c    Applied Statistics,
c    Volume 37, Number 3, pages 477-484, 1988.
c
c  Parameters:
c
c    Input, double precision P, the value of the cumulative probability
c    densitity function.  0 .lt. P .lt. 1.  If P is outside this range, an
c    "infinite" value will be returned.
c
c    Output, double precision X, the normal deviate value
c    with the property that the probability of a standard normal deviate being
c    less than or equal to the value is P.
c
      implicit none

      double precision a(8)
      double precision b(8)
      double precision c(8)
      double precision const1
      parameter ( const1 = 0.180625D+00 )
      double precision const2
      parameter ( const2 = 1.6D+00 )
      double precision d(8)
      double precision e(8)
      double precision f(8)
      double precision p
      double precision q
      double precision r
      double precision r8_huge
      double precision r8poly_value
      double precision split1
      parameter ( split1 = 0.425D+00 )
      double precision split2
      parameter ( split2 = 5.0D+00 )
      double precision x

      save a
      save b
      save c
      save d
      save e
      save f

      data a /
     &  3.3871328727963666080D+00,
     &  1.3314166789178437745D+02,
     &  1.9715909503065514427D+03,
     &  1.3731693765509461125D+04,
     &  4.5921953931549871457D+04,     
     &  6.7265770927008700853D+04,     
     &  3.3430575583588128105D+04,     
     &  2.5090809287301226727D+03 /
      data b /
     &  1.0D+00,
     &  4.2313330701600911252D+01,
     &  6.8718700749205790830D+02,  
     &  5.3941960214247511077D+03,
     &  2.1213794301586595867D+04,
     &  3.9307895800092710610D+04,
     &  2.8729085735721942674D+04,
     &  5.2264952788528545610D+03 /
      data c /
     &  1.42343711074968357734D+00,
     &  4.63033784615654529590D+00,
     &  5.76949722146069140550D+00,
     &  3.64784832476320460504D+00,
     &  1.27045825245236838258D+00,
     &  2.41780725177450611770D-01,
     &  2.27238449892691845833D-02,
     &  7.74545014278341407640D-04 /
      data d /
     &  1.0D+00,
     &  2.05319162663775882187D+00,
     &  1.67638483018380384940D+00,
     &  6.89767334985100004550D-01,
     &  1.48103976427480074590D-01,  
     &  1.51986665636164571966D-02,
     &  5.47593808499534494600D-04,    
     &  1.05075007164441684324D-09 /
      data e /
     &  6.65790464350110377720D+00,
     &  5.46378491116411436990D+00,
     &  1.78482653991729133580D+00,
     &  2.96560571828504891230D-01,
     &  2.65321895265761230930D-02,
     &  1.24266094738807843860D-03,
     &  2.71155556874348757815D-05,
     &  2.01033439929228813265D-07 /
      data f /
     &  1.0D+00,
     &  5.99832206555887937690D-01,
     &  1.36929880922735805310D-01,
     &  1.48753612908506148525D-02,
     &  7.86869131145613259100D-04,  
     &  1.84631831751005468180D-05,
     &  1.42151175831644588870D-07,    
     &  2.04426310338993978564D-15 /

      if ( p .le. 0.0D+00 ) then
        x = - r8_huge ( )
        return
      end if

      if ( 1.0D+00 .le. p ) then
        x = r8_huge ( )
        return
      end if

      q = p - 0.5D+00

      if ( abs ( q ) .le. split1 ) then

        r = const1 - q * q
        x = q * r8poly_value ( 8, a, r ) / r8poly_value ( 8, b, r )

      else

        if ( q .lt. 0.0D+00 ) then
          r = p
        else
          r = 1.0D+00 - p
        end if

        if ( r .le. 0.0D+00 ) then

          x = r8_huge ( )

        else

          r = sqrt ( - log ( r ) )

          if ( r .le. split2 ) then

            r = r - const2
            x = r8poly_value ( 8, c, r ) / r8poly_value ( 8, d, r )

          else

            r = r - split2
            x = r8poly_value ( 8, e, r ) / r8poly_value ( 8, f, r )

          end if

        end if

        if ( q .lt. 0.0D+00 ) then
          x = -x
        end if

      end if

      return
      end
	  
c*********************************************************************72	  
	  function r8poly_value ( n, a, x )

c*********************************************************************72
c
cc R8POLY_VALUE evaluates an R8POLY
c
c  Discussion:
c
c    For sanity's sake, the value of N indicates the NUMBER of
c    coefficients, or more precisely, the ORDER of the polynomial,
c    rather than the DEGREE of the polynomial.  The two quantities
c    differ by 1, but cause a great deal of confusion.
c
c    Given N and A, the form of the polynomial is:
c
c      p(x) = a(1) + a(2) * x + ... + a(n-1) * x^(n-2) + a(n) * x^(n-1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision A(N), the coefficients of the polynomial.
c    A(1) is the constant term.
c
c    Input, double precision X, the point at which the polynomial is
c    to be evaluated.
c
c    Output, double precision R8POLY_VALUE, the value of the polynomial at X.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8poly_value
      double precision x

      r8poly_value = a(n)
      do i = n - 1, 1, -1
        r8poly_value = r8poly_value * x + a(i)
      end do

      return
      end

c*********************************************************************72	  
	  function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end