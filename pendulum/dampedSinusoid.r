# This function returns a damped sinusoid model
#
# k = dampling
# t = multiplication model
# A = peak amptitude
# f = frequency
# phi = phase shift
# B = vertical shift
dampedSinusoid = function(t, k, A, f, phi, B)
{
  theta.hat = exp(-k * t) * (A * sin(2*pi * f * t - phi) + B )
  return(theta.hat)
}