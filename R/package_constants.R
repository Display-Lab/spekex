# Package Constants
SE <- new.env()

SE$SUITE_NAMES <- c('sham','va','mtx', 'aspire')

# Error messages
SE$UNRECOGNIZED_NAME <- "Unrecognized set name."
SE$NO_SPEK_MSG <- "No spek specified."

# IRIs for spek parsing.  Will be extracted to spektools
SE$INPUT_TABLE_IRI  <- "http://example.com/slowmo#InputTable"
SE$MEASURE_IRI      <- "http://example.com/slowmo#Measure"
SE$COLUMN_USE_IRI   <- "http://example.com/slowmo#ColumnUse"
SE$TABLE_SCHEMA_IRI <- "http://www.w3.org/ns/csvw#tableSchema"
SE$COLUMN_NAME_IRI  <- "http://www.w3.org/ns/csvw#name"
SE$TABLE_IRI        <- "http://www.w3.org/ns/csvw#Table"
SE$COLUMNS_IRI      <- "http://www.w3.org/ns/csvw#columns"
SE$DIALECT_IRI      <- "http://www.w3.org/ns/csvw#dialect"

SE$SPEK_IRI               <- "http://example.com/slowmo#spek"
SE$ABOUT_ORGANIZATION_IRI <- "http://example.com/slowmo#IsAboutOrganization" 
SE$ABOUT_MEASURE_IRI      <- "http://example.com/slowmo#IsAboutMeasure"
SE$ABOUT_TEMPLATE_IRI     <- "http://example.com/slowmo#IsAboutTemplate"
