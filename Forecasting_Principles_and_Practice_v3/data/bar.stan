functions {
    real gpareto_lpdf(vector y, real ymin, real k, real sigma) {
      // generalised Pareto log pdf 
      int N = rows(y);
      real inv_k = inv(k);
      if (k<0 && max(y-ymin)/sigma > -inv_k)
        reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
        return -(1+inv_k)*sum(log1p((y-ymin) * (k/sigma))) -N*log(sigma);
      else
        return -sum(y-ymin)/sigma -N*log(sigma); // limit k->0
    }
    real gpareto_cdf(vector y, real ymin, real k, real sigma) {
      // generalised Pareto cdf
      real inv_k = inv(k);
      if (k<0 && max(y-ymin)/sigma > -inv_k)
        reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
        return exp(sum(log1m_exp((-inv_k)*(log1p((y-ymin) * (k/sigma))))));
      else
        return exp(sum(log1m_exp(-(y-ymin)/sigma))); // limit k->0
    }
    real gpareto_lcdf(vector y, real ymin, real k, real sigma) {
      // generalised Pareto log cdf
      real inv_k = inv(k);
      if (k<0 && max(y-ymin)/sigma > -inv_k)
        reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
        return sum(log1m_exp((-inv_k)*(log1p((y-ymin) * (k/sigma)))));
      else
        return sum(log1m_exp(-(y-ymin)/sigma)); // limit k->0
    }
    real gpareto_lccdf(vector y, real ymin, real k, real sigma) {
      // generalised Pareto log ccdf
      real inv_k = inv(k);
      if (k<0 && max(y-ymin)/sigma > -inv_k)
        reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
        return (-inv_k)*sum(log1p((y-ymin) * (k/sigma)));
      else
        return -sum(y-ymin)/sigma; // limit k->0
    }
    real gpareto_rng(real ymin, real k, real sigma) {
      // generalised Pareto rng
      if (sigma<=0)
        reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
        return ymin + (uniform_rng(0,1)^-k -1) * sigma / k;
      else
        return ymin - sigma*log(uniform_rng(0,1)); // limit k->0
    }
   real gparetoJacP2L2(vector y, real ymin, real k, real sigma) {
       // L2 Jacobian for GPD with ymin known
      int N = rows(y);
      real inv_k = inv(k);
      real prejac1 = sum((y-ymin).*(y-ymin)) * sum((1+(k/sigma)*(y-ymin)).*(1+(k/sigma)*(y-ymin)).*log1p((k/sigma)*(y-ymin)).*log1p((k/sigma)*(y-ymin)))
                     -sum((y-ymin).*(1+(k/sigma)*(y-ymin)).*log1p((k/sigma)*(y-ymin)))*sum((y-ymin).*(1+(k/sigma)*(y-ymin)).*log1p((k/sigma)*(y-ymin)));
      real prejac2=sum((y-ymin).*(y-ymin))*sum((y-ymin).*(y-ymin).*(y-ymin).*(y-ymin))
               -sum((y-ymin).*(y-ymin).*(y-ymin))*sum((y-ymin).*(y-ymin).*(y-ymin));
      if (k<0 && max(y-ymin)/sigma > -inv_k)
          reject("k<0 and max(y-ymin)/sigma > -1/k; found k, sigma =", k, sigma)
      if (sigma<=0)
         reject("sigma<=0; found sigma =", sigma)
      if (fabs(k) > 1e-15)
         return -log(k*k)+0.5*log(prejac1);
      else
         return -log(2)-2*log(sigma)+0.5*log(prejac2); // limit k->0
      }
}
data {
  real ymin;
  int<lower=0> N;
  vector<lower=ymin>[N] y;
}
transformed data {
  real ymax = max(y);
}
parameters {
  real<lower=0> sigma; 
  //real k;
  real<lower=-sigma/(ymax-ymin)> k; 
}
model {
  y ~ gpareto(ymin, k, sigma);
  target += gparetoJacP2L2(y, ymin, k, sigma); //Fiducial Jacobian
}
generated quantities {
  real<lower=0> y_sim=gpareto_rng(ymin, k, sigma);
}

