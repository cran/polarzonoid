\name{slerp}

\alias{slerp}

\title{
Spherical Linear Interpolation (slerp) on the Unit Sphere
}

\description{
Let \eqn{u_0} and \eqn{u_1} be two unit vectors on the unit sphere \eqn{\mathbb{S}^{m-1}}, that are not antipodal.

The  purpose of \code{slerp()}is to compute points along the great circle arc from \eqn{u_0} to \eqn{u_1},
from user supplied interpolation parameters \eqn{\tau}.
The value \eqn{\tau{=}0} produces \eqn{u_0} and \eqn{\tau{=}1} generates \eqn{u_1}.
For example to generate \code{n} uniformly space points,
set \code{tau = (0:(n-1))/(n-1) }.

If \code{tau=NULL}, the points are uniformly spaced and their number is computed automatically,
based on the user-supplied parameter \code{thetamax}; see \bold{Details}.
}

\usage{
slerp( u0, u1, tau=NULL, thetamax=pi/36 )
}

\arguments{
\item{u0, u1}{
numeric vectors with same length (verified) and unit length (not verified).
If they are antipodal, the great circle arc is undefined and the
function returns \code{NULL}.
}

\item{tau}{
a numeric vector of interpolation parameters.
The default is \code{NULL} and then \code{tau} is computed using \code{thetamax}
and the angle between \code{u0} and \code{u1}.
If \code{tau} is not \code{NULL}, then \code{thetamax} is ignored.
}

\item{thetamax}{
the maximum angular step between successive points on the arc.
This value must be positive.
The default corresponds to 5\eqn{^\circ}.
}
}



\value{
\code{slerp()} returns an \code{n x m} matrix with computed points
in the rows.
\code{n} is the number of points, and
\code{m} is the length of \code{u0} and \code{u1}.
But if \code{n=1}, it returns a plain \code{m}-vector.

If \code{tau} is not \code{NULL}, \code{n=length(tau)}.
But if \code{length(tau)==0}, the function returns \code{NULL}.

If \code{tau} is \code{NULL} (the default),
\code{n = ceiling(theta/thetamax) + 1},
where \code{theta} is the angle between \code{u0} and \code{u1}.
If \code{u0} and \code{u1} are equal, then \code{n=1}.

In case of error, e.g. \code{u0} and \code{u1} are antipodal,
the function returns \code{NULL}.
}


\details{
If \bold{both} \code{thetamax} and \code{tau} are not \code{NULL},
\code{tau} has priority and \code{thetamax} is ignored.

When \code{tau=NULL},
the angle step \eqn{\Delta \theta} is computed to be
as large as possible and also satisfy \eqn{\Delta \theta \le \theta_{max}}.
If the distance between \eqn{u_0} and \eqn{u_1} is \eqn{\theta},
the number of points is \eqn{n = \text{ceiling}( \theta / \theta_{max}) + 1}
and \eqn{\Delta \theta = \theta / (n-1)}.

The interpolation formula is not linear; it uses the \code{sin()} function.
For details see \bold{Shoemake}.
}

\references{
Shoemake, Ken.
\emph{Animating Rotation with Quaternion Curves}.
pp. 245-254.
SIGGRAPH 1985.
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{adaptation}% use one of  RShowDoc("KEYWORDS")

