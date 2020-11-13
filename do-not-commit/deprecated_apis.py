# ruleid:bokeh-deprecated-apis
from bokeh.layouts import widgetbox
from bokeh.models.graphs import from_networkx
# ruleid:bokeh-deprecated-apis

widgetbox(children=[slider], sizing_mode="scale_width")

graph_renderer = from_networkx(G, nx.spring_layout, scale=0.5, center=(0, 0))
