stepwise = function(formula, data, alpha.enter = 0.15, alpha.remove = 0.15, ...) {
  mf = match.call(expand.dots = FALSE)
  mf[[1]] = quote(model.frame)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf = mf[c(1, m)]
  mf = eval(mf)
  
  mt <- attr(mf, "terms")
  y <- mf[attr(mt, "response")]
  x = mf[attr(mt, "term.labels")]
  
  extras = pairlist(...)
  if (is.null(extras)) {
    extras = pairlist(model = NULL, sse = sum((y - mean(unlist(y))) ^ 2), step = 1)
    cat('\n', 'Call:', '\n', sep = '')
    print(match.call())
  }
  
  cat('\n', 'Step ', extras$step, ':', '\n', sep = '')
  
  # Enter
  enter = mapply(function(x, name) {
    x = as.data.frame(x)
    names(x) = name
    A = if (is.null(extras$model)) cbind(y, x) else cbind(y, extras$model, x)
    cm = model.frame(A)
    cmt = attr(cm, "terms")
    clm = lm.fit(model.matrix(cmt, cm), model.response(cm))
    csse = sum(clm$residuals ^ 2)
    f = (extras$sse - csse) / csse * clm$df.residual
    c(csse, clm$df.residual, f, pf(f, 1, clm$df.residual, lower.tail = FALSE))
  }, x, names(x))
  enter = t(enter)
  colnames(enter) = c('SSE', 'df', 'F', 'p')
  maxv = enter[order(enter[,'F'], decreasing = TRUE)[1], ,drop = FALSE]
  if (maxv[, 'F'] >= qf(alpha.enter, 1, maxv[, 'df'], lower.tail = FALSE)) {
    extras$model = if (is.null(extras$model)) x[rownames(maxv)] else cbind(extras$model, x[rownames(maxv)])
    x[rownames(maxv)] = NULL
  } else {
    rownames(enter) = sapply(rownames(enter), function(x) { toString(c(names(extras$model), x)) })
    print(enter)
    cat('\nBest model:\n')
    formula = paste(names(y), paste(names(extras$model), collapse = ' + '), sep = ' ~ ')
    cat(formula)
    clm = lm(formula, data = cbind(y, extras$model))
    cat('\n\nCoefficients:\n')
    print(clm$coefficients)
    return(invisible())
  }
  rownames(enter) = sapply(rownames(enter), function(x) { toString(c(names(extras$model)[-length(extras$model)], x)) })
  print(enter)
  cat('\n', 'Adding ', rownames(maxv), '\n', sep = '')
  
  # Delete
  while (length(extras$model) > 0) {
    # Full model
    A = cbind(y, extras$model)
    cm = model.frame(A)
    cmt = attr(cm, 'terms')
    flm = lm.fit(model.matrix(cmt, cm), model.response(cm))
    fsse = sum(flm$residuals ^ 2)
    
    if (length(extras$model) < 2) {
      f = (sum((model.response(cm) - mean(model.response(cm))) ^ 2) - fsse) / fsse * flm$df.residual
      minv = structure(c(fsse, flm$df.residual, f, pf(f, 1, flm$df.residual, lower.tail = FALSE)), dim = c(1, 4), dimnames = list(names(extras$model), c('SSE', 'df', 'F', 'p')))
      minf = 1
      cat('\n')
      print(minv)
    } else {
      delete = sapply(seq_along(extras$model), function(x) {
        A = cbind(y, extras$model[-x])
        cm = model.frame(A)
        cmt = attr(cm, 'terms')
        clm = lm.fit(model.matrix(cmt, cm), model.response(cm))
        csse = sum(clm$residuals ^ 2)
        f = (csse - fsse) / fsse * flm$df.residual
        c(csse, flm$df.residual, f, pf(f, 1, flm$df.residual, lower.tail = FALSE))
      })
      delete = t(delete)
      colnames(delete) = c('SSE', 'df', 'F', 'p')
      rownames(delete) = sapply(seq_along(extras$model), function(x) { toString(names(extras$model)[-x]) })
      cat('\n')
      print(delete)
      minf = order(delete[, 'F'])[1]
      minv = delete[minf, ,drop = FALSE]
    }
    if (minv[, 'F'] < qf(alpha.remove, 1, minv[, 'df'], lower.tail = FALSE)) {
      cat('\n', 'Removing ', names(extras$model)[minf], '\n', sep = '')
      x = cbind(x, extras$model[minf])
      extras$model = extras$model[-minf]
    } else {
      break
    }
  }
  cat('\nCurrent model:\n')
  formula = paste(names(y), paste(names(extras$model), collapse = ' + '), sep = ' ~ ')
  cat(formula)
  clm = lm(formula, data = cbind(y, extras$model))
  cat('\n\nCoefficients:\n')
  print(clm$coefficients)
  formula = paste(names(y), paste(names(x), collapse = ' + '), sep = ' ~ ')
  data = cbind(y, x)
  stepwise(formula, data = data, alpha.enter, alpha.remove,
           model = extras$model, sse = sum(clm$residuals ^ 2), step = extras$step + 1)
}
