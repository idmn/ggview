# printing lists the collection and its parameters

    Code
      print(plots_init())
    Message
      -- <plots_tbl> ------------------------------------------------------ 0 plots --
      Empty — add a plot with `plots_append()`.
    Code
      print(plots)
    Message
      -- <plots_tbl> · Test bundle ---------------------------- 2 plots in 1 folder --
      Module 1
       1* first  bar     4 x 8   Changed
       2  second heatmap 16 x 10 <no title>
      1 plot without a title.
      1 plot changed · `plots_reset()` forgets it
      `plots_get()` renders one · `as.gallery()` browses them all
    Code
      print(plots_params(plots, "Module 1/first"))
    Message
      -- <plots_params> --------------------------------------------- 19 parameters --
        title            text   Title -> Changed
        title_size       number 13.2
        subtitle         text   Subtitle
        subtitle_size    number 11
        caption          text   Caption
        caption_size     number 8.8
        x                text   wt
        x_size           number 11
        y                text   mpg
        y_size           number 11
        axis_text_size   number 8.8
        axis_text_wrap   number 
        text_size        number 11
        legend           choice <none>  (right, left, top, bottom, none)
        legend_direction choice <none>  (horizontal, vertical)
        x_grid           flag   TRUE
        y_grid           flag   TRUE
        width            number 12 -> 4
        height           number 8
    Code
      print(plots_dims(heatmap = c(18, 11)))
    Message
      -- <plots_dims> ------------------------------------------------------ inches --
        bar          12 x 8
        diverging    15 x 8
        dodged_bar   15 x 9
        dumbbell     16 x 9
        heatmap      18 x 11
        line         15 x 9
        stacked_bar  15 x 9
        .default     15 x 9
    Code
      print(customizer_default())
    Message
      -- <plots_customizer> ----------------------------------------------------------
        parameters: legend_labels, x_labels, y_labels, title, title_size, subtitle, subtitle_size, caption, caption_size, x, x_size, y, y_size, axis_text_size, axis_text_wrap, text_size, data_label_size, legend, legend_direction, x_grid, y_grid, colors

