import re


def parse_capacity_series(row):
    pattern = r'(\w+)="([^"]+)"'
    matches = re.findall(pattern, row)
    return {key: value for key, value in matches}


def get_da_gate_closure(col):
    return (
        col.dt.convert_time_zone("CET")
        .dt.replace(hour=12, minute=0, second=0, microsecond=0)
        .dt.convert_time_zone("UTC")
    )


def get_outage_effective_max_end(col, after_da: bool):
    """
    If after_da = True, an outage can have effects until max. the end of the next day because the DA auction
    would have closed already. If the outage starts before DA, there would be no effect after the end of that day
    (i.e. effective max end = the end of the day of col).
    """
    if after_da:
        return (
            col.dt.convert_time_zone("CET")
            .dt.truncate("1d")
            .dt.offset_by("2d")
            .dt.convert_time_zone("UTC")
        )
    else:
        return (
            col.dt.convert_time_zone("CET")
            .dt.truncate("1d")
            .dt.offset_by("1d")
            .dt.convert_time_zone("UTC")
        )
