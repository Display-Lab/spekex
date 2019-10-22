#' Package Constants
#' @export SE
SE <- new.env()

SE$SUITE_NAMES <- c('sham','va','mtx', 'aspire', 'va2')

# Error messages
SE$UNRECOGNIZED_NAME <- "Unrecognized set name."
SE$NO_SPEK_MSG <- "No spek specified."
SE$MEASURE_NOT_IN_SPEK <- "Measure not found in spek."

# IRIs for spek parsing.  Will be extracted to spektools
SE$INPUT_TABLE_IRI  <- "http://example.com/slowmo#InputTable"
SE$MEASURE_IRI      <- "http://purl.obolibrary.org/obo/psdo_0000102"
SE$COLUMN_USE_IRI   <- "http://example.com/slowmo#ColumnUse"
SE$TABLE_SCHEMA_IRI <- "http://www.w3.org/ns/csvw#tableSchema"
SE$COLUMN_NAME_IRI  <- "http://www.w3.org/ns/csvw#name"
SE$COLUMN_DTYPE_IRI <- "http://www.w3.org/ns/csvw#datatype"
SE$COLUMN_TITLE_IRI <- "http://www.w3.org/ns/csvw#title"
SE$DESCRIPTION_IRI  <- "http://purl.org/dc/terms/description"
SE$TABLE_IRI        <- "http://www.w3.org/ns/csvw#Table"
SE$COLUMNS_IRI      <- "http://www.w3.org/ns/csvw#columns"
SE$DIALECT_IRI      <- "http://www.w3.org/ns/csvw#dialect"
SE$DC_TITLE_IRI     <- "http://purl.org/dc/terms/title"
SE$SCHEMA_NAME_IRI  <- "http://schema.org/name"
SE$SCHEMA_IDENTIFIER_IRI <- "http://schema.org/identifier"
SE$WITH_COMPARATOR_IRI   <- "http://example.com/slowmo#WithComparator"

SE$SPEK_IRI           <- "http://example.com/slowmo#spek"
SE$ABOUT_MEASURE_IRI  <- "http://example.com/slowmo#IsAboutMeasure"
SE$ABOUT_TEMPLATE_IRI <- "http://example.com/slowmo#IsAboutTemplate"
SE$HAS_CANDIDATE_IRI  <- "http://example.com/slowmo#HasCandidate"
SE$COMPARISON_VALUE   <- "http://example.com/slowmo#ComparisonValue"
SE$ABOUT_ORGANIZATION_IRI <- "http://example.com/slowmo#IsAboutOrganization"

### Additional IRIs used in bitstomach ###
SE$DESCRIBED_BY <- "http://example.com/slowmo#DescribedBy"
SE$REGARDING_MEASURE <- "http://example.com/slowmo#RegardingMeasure"
SE$DEFAULT_APP_IRI <- "http://example.com/app#"
SE$HAS_PERFORMERS_IRI <- "http://example.com/slowmo#IsAboutPerformer"
SE$PERFORMER_IRI <- "http://purl.obolibrary.org/obo/psdo_0000085"

# Slowmo ascribee IRIs
SE$CAPABILITY_BARRIER_IRI <- "http://example.com/slowmo#CapabilityBarrier"
SE$NEGATIVE_TREND_IRI     <- "http://purl.obolibrary.org/obo/psdo_0000100"
SE$POSITIVE_TREND_IRI     <- "http://purl.obolibrary.org/obo/psdo_0000099"
SE$NEGATIVE_GAP_IRI       <- "http://purl.obolibrary.org/obo/psdo_0000105"
SE$POSITIVE_GAP_IRI       <- "http://purl.obolibrary.org/obo/psdo_0000104"
SE$PERFORMANCE_GAP_IRI    <- "http://purl.obolibrary.org/obo/psdo_0000106"
SE$LARGE_GAP_IRI          <- "http://example.com/slowmo#LargeGap"

# Additional preconditions from knowledge base
SE$ACHEIVEMENT_IRI         <- "http://example.com/slowmo#Achievement"
SE$CONSEC_NEG_GAP_IRI      <- "http://example.com/slowmo#ConsecutiveNegativePerformanceGapContent"
SE$CONSEC_POS_GAP_IRI      <- "http://example.com/slowmo#ConsecutivePositivePerformanceGapContent"
SE$GOAL_COMPARATOR_IRI     <- "http://purl.obolibrary.org/obo/psdo_0000094"
SE$SOCIAL_COMPARATOR_IRI   <- "http://purl.obolibrary.org/obo/psdo_0000095"
SE$STANDARD_COMPARATOR_IRI <- "http://purl.obolibrary.org/obo/psdo_0000096"

# DEPRECATED
SE$HAS_DISPOSITION_IRI <- "http://purl.obolibrary.org/obo/RO_0000091"
