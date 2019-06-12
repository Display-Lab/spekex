# Spek suite components
#  A la carte peices from each suite

SHAM_SPEK_COL_LIST <- list(list(`http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "string")),
                                `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "performer")),
                                `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Name")),
                                `http://purl.org/dc/terms/description` = list(list(`@value` = "Performer unique ID")),
                                `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "identifier"))),
                           list(`http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
                                `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "timepoint")),
                                `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Time")),
                                `http://purl.org/dc/terms/description` = list(list(`@value` = "Time at which performance was measured.")),
                                `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "time"))),
                           list(`http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
                                `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "performance")),
                                `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Performance")),
                                `http://purl.org/dc/terms/description` = list(list(`@value` = "Demonstration performance value")),
                                `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "value"))))

