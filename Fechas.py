import pandas as pd
import dash
from dash import dash_table
from dash.dependencies import Input, Output
from datetime import datetime as dt
from dash import dcc
from dash import html
df = pd.read_excel("C:/Users/JosePinzon/Documents/FINTRADE/Python/Archivo2021.xlsx") #Cargar el archivo exportado anteriormente

df.head()
#df = df[(df["FECHA ASAMBLEA"] != '-')]
#df.dropna(axis = 0, inplace = True)
df['FECHA ASAMBLEA'] = pd.to_datetime(df['FECHA ASAMBLEA'])

app = dash.Dash()

app.layout = html.Div([
    html.H1("Filtro de fechas", style={"text-align":"center"}),
    
    dcc.DatePickerRange(
            id="my-date-picker-range",
            min_date_allowed=dt(2021, 1, 10),
            max_date_allowed=dt(2021, 8, 26),
            initial_visible_month=dt(2021, 3, 10),
            clearable=True,
            number_of_months_shown=2,
        ),
    html.Br(), #Espacio entre la fecha y la tabla
    html.Br(),
    
        
    dash_table.DataTable(
    id='Tabla',
    columns=[{"name": i, "id": i} for i in df.columns],
    data=df.to_dict('records'),
    page_action="native",  #Permite actualizar con Fecha
    page_size=20)
  
    ]
)

def date_string_to_date(date_string):
    return pd.to_datetime(date_string, infer_datetime_format=True)


@app.callback(
    dash.dependencies.Output("Tabla", "data"), #modificar datatable con "data"
    [dash.dependencies.Input("my-date-picker-range", "start_date"),#modificar fechas con el inicio
    dash.dependencies.Input("my-date-picker-range", "end_date")])#modificar fechas con el final

def update_data(start_date, end_date): #funcion para actualizar
    data = df.to_dict("records") #df a diccionario
    if start_date and end_date:
        mask = (date_string_to_date(df["FECHA ASAMBLEA"]) >= date_string_to_date(start_date)) & (
            date_string_to_date(df["FECHA ASAMBLEA"]) <= date_string_to_date(end_date)
        ) #Condicionales para filtrado de fecha
        data = df.loc[mask].to_dict("records") #Aplicar el filtro sobre el df y la tabla
    return data #retorne el df


if __name__ == "__main__":
    app.run_server()
    
