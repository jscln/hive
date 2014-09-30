Hive-Plots-Sandbox
==================

My hive plot visualization of a non-profit organization’s partners, based on survey data. Each node is a partner, and the edges between two nodes are weighted by the number of responses the partners had in common; edge weights were normalized. Nodes’ colors and placements were determined by their eigenvectors, and their sizes by closeness centrality. Edges’ colors and widths corresponded to their normalized weights. Axes were assigned by hierarchical clusterings of average shortest paths (counter-clockwise from top). The R programming was poached heavy-handedly from Vessy’s Les Mis example: http://www.vesnam.com/Rblog/viznets3/. 
