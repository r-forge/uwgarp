

# American call
am.call <- optionSpec(style="american", type="call")
am.call.val <- optionValue(am.call, N=4)

# American put
am.put <- optionSpec(style="american", type="put")
am.put.val <- optionValue(am.put)

# European call
euro.call <- optionSpec(style="european", type="call", S0=810, K=800, 
                        maturity=0.5, r=0.05, volatility=0.2, q=0.02)
euro.call.val.bs <- optionValue(euro.call, method="Black-Scholes")
euro.call.val.bin <- optionValue(euro.call, method="Binomial", N=100)

# European put
euro.put <- optionSpec(style="european", type="put")
euro.put.val.bs <- optionValue(euro.put, method="Black-Scholes") 
euro.put.val.bin <- optionValue(euro.put, method="Binomial") 
