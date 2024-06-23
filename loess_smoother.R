ev <- readRDS("./data/electric_vehicle_eu.rds")
#loess
ev_smooth <- loess(
    data = ev, #dataset()
    price ~ battery_size, # input$var / ~get(input$var) / dataset()[[, input$var]]
    span = .75
    )
ev_pred <- predict(ev_smooth, se = T)

ev_df = data.frame(x = ev_smooth$x,
                   fit = ev_pred$fit,
                   lb = ev_pred$fit - (1.96 * ev_pred$se),
                   ub = ev_pred$fit + (1.96 * ev_pred$se))
ev_df = ev_df[order(ev_df$battery_size),] # battery_size ???
line_fmt = list(dash="solid", width = 1.5, color=NULL)

#loess
fig <- plot_ly(
    data = ev, #dataset()
    x = ~battery_size, #input$var
    y = ~price,
    #color = ~manufacturer,
    type = 'scatter',
    mode = 'markers'
)
fig
#loess - band
fig <- fig %>% add_ribbons(
    x = ev_df$battery_size, #input$var ???
    ymin = ev_df$lb,
    ymax = ev_df$ub,
    name = '95% CI',
    line = list(color = 'rgb(180, 180, 180)', width = 0),
    fillcolor = "gray20"
)
fig
"#366092"
#loess - line
fig <- fig %>% add_lines(
    x = ~battery_size,
    y = ev_pred$fit, name = "Mean",
    line=list(color = 'rgb(63, 63, 63)', width=2)
)

fig
