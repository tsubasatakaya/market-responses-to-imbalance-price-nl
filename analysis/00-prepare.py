import os
import pendulum
from src.preprocess.create_base_15min_data import create_base_15min_data

output_path = "output"
output_data_path = os.path.join(output_path, "data")
output_figure_path = os.path.join(output_path, "figures")
output_table_path = os.path.join(output_path, "tables")
output_model_path = os.path.join(output_path, "models")

for path in [output_data_path, output_figure_path, output_table_path, output_model_path]:
    if not os.path.exists(path):
        os.makedirs(path)

start = pendulum.datetime(2021, 1, 1, tz="CET").in_timezone("UTC")
end = pendulum.datetime(2024, 10, 1, tz="CET").in_timezone("UTC")

create_base_15min_data(start, end)


