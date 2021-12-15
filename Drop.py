import dash
import pandas as pd
import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output
from dash import dash_table

df = pd.read_excel("C:/Users/JosePinzon/Documents/FINTRADE/Python/Archivo2021.xlsx") #Cargar el archivo exportado anteriormente



app = dash.Dash(__name__)


app.layout = html.Div([
  dcc.Dropdown(id='emisor', multi=True,
  options=[{'label': x, 'value': x} for x in sorted(df.EMISOR.unique())],
  clearable=True,
  placeholder="Seleccione el Emisor de la accion"),

  html.Div(id='dd-output-container'),
  
  dash_table.DataTable(
      id='Tabla',
      columns=[{"name": i, "id": i} for i in df.columns],
      data=df.to_dict('records'),
      page_action="native",  #Permite actualizar con Fecha
      page_size=20)
      
])


@app.callback(
    Output('Tabla', 'data'),
    [Input('emisor', 'value')],
    prevent_initial_call=False
)

def update_rows(selected_value):

  if len(selected_value) == 0:
    data=df.to_dict('records')
    return data
  elif len(selected_value) > 0:
    data = df[df['EMISOR'].isin(selected_value)].to_dict("records") #Aplicar el filtro sobre el df y la tabla
    return data #retorne el df



if __name__ == '__main__':
    app.run_server()
    
    
