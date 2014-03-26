
function M = dist (V)

  ## usage: M = dist (V)
  ##
  ## compute the distance matrix of the given row vectors

  if nargin != 1
    usage("dist (V)");
  elseif !ismatrix(V)
    error("dist: argument must be a matrix")
  endif

  n = rows(V); k = columns(V);

  M = zeros(n,n);
  for i = 1:n
    for j = 1:n
      M(i,j) = sqrt( (V(i,:)-V(j,:)) * (V(i,:)-V(j,:))' );
    endfor
  endfor

endfunction

function S = stress1 (M0,M)

  ## Usage: stress1 (M0,M)
  ##
  ## Given a base metric M0 and a second metric M, determines the
  ## "stress" of M relative to M0, which measures how much the distances
  ## in M deviate from the base metric. More precisely, this is also
  ## known as "stress-1," see Borg, Groenen: Modern Multidimensional 
  ## Scaling, Springer, 1997.

  if nargin != 2
    usage("stress1 (M0,M)")
  elseif !ismatrix(M0) || !ismatrix(M) || rows(M) != columns(M) || size(M) != size(M0)
    error("stress1: arguments must be compatible square matrices");
  endif

  S = sqrt(sum(sumsq(M-M0))/sum(sumsq(M0)));

endfunction

function [V,S,niter] = smacof (M,V0)

  ## Usage: [V,S] = smacof (M,V0)
  ##
  ## Multi-dimensional scaling using the SMACOF algorithm: Compute an
  ## embedding V of n points in k-dimensional Euclidean space for the
  ## given metric M, where M is an n x n matrix. The n x k matrix V0
  ## denotes an initial (random or other) embedding used as the starting
  ## point of the algorithm. The number k of columns of V0 also
  ## determines the dimension of the embedding. If S is given, it returns
  ## the stress of the computed solution. If, in addition, niter is
  ## given, it returns the number of iterations performed.
  ##
  ## The algorithm is essentially a steepest descent procedure using a
  ## parametric stress majorizing function, and converges linearly to
  ## a local minimum of the stress function. See, e.g., Borg, Groenen:
  ## Modern Multidimensional Scaling, Springer, 1997, for details.
  ##
  ## Termination of the algorithm is controlled by two global parameters:
  ## smacof_eps and smacof_maxiter. The algorithm terminates as soon as
  ## either the stress of two successive solutions differs by less than
  ## smacof_eps, or smacof_maxiter iterations have been performed.
  ## If smacof_maxiter is set to zero or negative then only the smacof_eps
  ## criterion is used. Note that if in this case smacof_eps is set too
  ## small then the algorithm may loop forever.

  global smacof_eps = 1e-8;
  global smacof_maxiter = 1000;
  global logging = 0;

  ## check arguments
  if nargin != 2
    usage("smacof (M,V0)")
  elseif !ismatrix(M) || rows(M) != columns(M)
    error("smacof: 1st argument must be a square matrix");
  elseif !ismatrix(V0) || rows(M) != rows(V0)
    error("smacof: 2nd argument must be a compatible embedding");
  endif

  ## n = number of points, k = dimension of the target space
  n = rows(M);
  k = columns(V0);

  ## compute distance matrix and raw stress of the start solution
  D0 = dist(V0);
  s0 = sum(sumsq(D0-M))/2;

  ## set up the transformation matrix
  B = zeros(n, n);

  ## intialize iteration counter
  iter = 0;

  ## enter loop of iterative improvement steps
  while 1
    iter = iter + 1;
    ## compute the transformation matrix
    for i = 1:n
      for j = 1:n
	if i != j && D0(i,j) != 0
	  B(i,j) = -M(i,j)/D0(i,j);
	else
	  B(i,j) = 0;
	endif
      endfor
    endfor
    for i = 1:n
      B(i,i) = -sum(B(i,1:n));
    endfor
    ## compute the Guttman transform
    V = (B/n)*V0;
    ## compute distance matrix and raw stress of the new solution
    D = dist(V);
    s = sum(sumsq(D-M))/2;
    ## give feedback
    if logging!=0 && rem(iter,10)==0
      pure_call('log_cb', iter, s);
    endif
    ## check termination criteria
    if s0 - s < smacof_eps
      break;
    elseif smacof_maxiter > 0 && iter >= smacof_maxiter
      break;
    endif
    ## enter next iteration
    V0 = V; D0 = D; s0 = s;
  endwhile

  if nargout >= 2
    S = stress1(M,dist(V));
  endif
  if nargout >= 3
    niter = iter;
  endif

endfunction

function W = parot (V)

  ## Usage: W = parot (V)
  ##
  ## Compute a principle axis (PA) rotation of the points given by the
  ## rows of V. The result is a rotation of V s.t. all points lie
  ## closest to the first axis, the second axis accounts for most of the
  ## point scatter orthogonal to the first axis, etc. Computed using a
  ## singular value decomposition of V. See, e.g., Borg, Groenen:
  ## Modern Multidimensional Scaling, Springer, 1997, for details.

  if nargin != 1
    usage("parot (V)")
  elseif !ismatrix(V)
    error("parot: argument must be a matrix");
  endif

  [P,D,Q] = svd(V,1);
  W = V*Q;

  if columns(W) < columns(V)
    W = [W zeros(rows(W), columns(V)-columns(W))];
  endif

endfunction

function [V,S] = mds(M,K)

  r = 2*max(max(abs(M)));
  [V,S] = smacof (M,r*rand(rows(M),K));
  V = parot(V);

endfunction
