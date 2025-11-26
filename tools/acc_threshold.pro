function acc_threshold, data, target_prob
  on_error, 2
  compile_opt idl2, hidden

  ndata = n_elements(data)

  ; Calculate histogram
  pdf_counts = histogram(data, locations = xbin)

  ; Convert to probability density (area under curve = 1)
  bin_width = xbin[1] - xbin[0] ; Assuming uniform bins
  pdf_prob = pdf_counts / (total(pdf_counts) * bin_width)

  ; Calculate CDF correctly
  cdf = total(pdf_counts, /cumulative) / total(pdf_counts)

  idx_95 = where(cdf ge target_prob, count)
  ; print, xbin[idx_95[0]]
  ; plot, xbin, cdf, /ystyle, title = 'Cumulativres Density Function', $
  ; xtitle = 'Data Values', ytitle = 'Probability Density'

  ; oplot, xbin, cdf, linestyle = 1
  return, xbin[idx_95[0]]
end
