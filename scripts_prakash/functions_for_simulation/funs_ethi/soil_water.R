soil_water<-function(m, ff, sol, rho_b1){
  
  ## Read texture 
  silt0 = unlist(sol$SLSI)[1:3]
  clay0 = unlist(sol$SLCL)[1:3]
  sand0 = 100 - (silt0 + clay0)  
  
  ## Read SLOC, SSAT, SDUL, SLLL, SSKS and SBDM
  OC0 <- unlist(sol$SLOC)[1:3]; SSAT0 <- unlist(sol$SSAT)[1:3]; SDUL0<-unlist(sol$SDUL)[1:3] 
  SLLL0 <- unlist(sol$SLLL)[1:3]; SSKS0 <- unlist(sol$SSKS)[1:3]; rho_b0 = unlist(sol$SBDM)[1:3] 
  
  ## extract unit value of each
  rho_b0=rho_b0[m]; rho_b=rho_b1[m]; sand=sand0[m]; clay=clay0[m]; silt=silt0[m]
  OC=OC0[m]; SLLL=SLLL0[m]; SDUL=SDUL0[m]; SSAT=SSAT0[m]; SSKS=SSKS0[m]
  
  ## rho_b original
  th_s0 = -0.3334*rho_b0 + 0.0005*clay + 0.8945
  th_r0 = 0.0115*rho_b0*clay^0.7489
  alpha0 = (0.0012*sand + 0.0001*clay + 0.0089*OC + 0.0101)*rho_b0^-2.5325
  n0 = (-0.0034*sand - 0.0186*clay - 0.0351*OC + 1.1477)*rho_b0 + (0.0068*sand + 0.0217*clay + 0.0047*OC + 0.0080)
  
  th_fc0 <- th_r0 + (th_s0 - th_r0)/(1 + (alpha0*330)^n0)^(1-(1/n0)) ## Field capacity
  th_pwp0 <- th_r0 + (th_s0 - th_r0)/(1 + (alpha0*15000)^n0)^(1-(1/n0)) ## permanent wilting point
  
  ## equations for conductivity from Saxton & Rawl, 1980
   ## using original fc, pwp, ssat
  b0=(log(1500)-log(33))/(log(SDUL)-log(SLLL))
  g0=1/b0
  x0=3-g0
  ks_calc0=(1930*(SSAT - SDUL)^x0)/10
  
  ## Jabro 1992
  #ks_calc0=exp(9.56 - 0.81*log(silt) - 1.09*log(cly) - 4.64*rho_b0)*10
  
  ##################################################################################
  ## rho_b varying Zhengchao Tian et al. (2021)
  th_s = -0.3334*rho_b + 0.0005*clay + 0.8945
  th_r = 0.0115*rho_b*(clay^0.7489)
  alpha = (0.0012*sand + 0.0001*clay + 0.0089*OC + 0.0101)*rho_b^-2.5325
  n = (-0.0034*sand - 0.0186*clay - 0.0351*OC + 1.1477)*rho_b + (0.0068*sand + 0.0217*clay + 0.0047*OC + 0.0080)
  
  th_fc <- th_r + (th_s - th_r)/(1 + (alpha*330)^n)^(1-(1/n)) ## Field capacity
  th_pwp <- th_r + (th_s - th_r)/(1 + (alpha*15000)^n)^(1-(1/n)) ## permanent wilting point
  
  ## equations for conductivity
  b=(log(1500)-log(33))/(log(th_fc)-log(th_pwp))
  g=1/b
  x=3-g
  ks_calc=(1930*(th_s - th_fc)^x)/10
  
  ## Jabro 1992
  #ks_calc=exp(9.56 - 0.81*log(silt) - 1.09*log(cly) - 4.64*rho_b)*10
  
  ## calculate bias between calculated values and original values in .SOL file
  ## 1. SSAT
  #del_st <- th_s0 - SSAT ## need to subtract this bias from the calculated th_s
  
  ## 2. SUL
   del_ul = th_fc0 - SDUL ## need to subtract this bias from the calculated ??_fc
  
  ## 3. SLLL
  del_ll <- th_pwp0 - SLLL ## need to subtract this bias from the calculated ??_pwp
  
  #### 4. Saturated hydraulic conductivity Saxton and Rawls (2006)).
  del_ks = ks_calc0 - SSKS
  
  ## Bias correction
  #th_s1 <- th_s - del_st
  th_fc1 <- th_fc - del_ul
  
  ## Find ratio of th_s and th_fc
  if(th_s>th_fc){
    prop=th_s/th_fc
  } else {
    prop=SSAT/SDUL ## in case calculated field capacity is smaller than sat 
  }
  
  th_s1 <- prop*th_fc1 ## Bias corrected field capacity
  
  th_pwp1 <- th_pwp - del_ll
  ks1 <- ks_calc - del_ks
   
  ### make a list to return
  list<- list(th_s1, th_fc1, th_pwp1, ks1, rho_b1)
  
  return(list)
}