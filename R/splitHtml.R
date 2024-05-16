# # WB::splitHtml("bla.html",  "path/to/folder/")


`splitHtml` <- function(html, dir, ...) {

#     on.exit(suppressWarnings(sink()))


#     header <- c(
#         "@extends('layouts.master')",
#         "",
#         "@section('pageTitle', $instrumentName. ' - Sectiunea '.$sectionNumber)",
#         "",
#         "@section('customCSS')",
#         "<!-- Select2 -->",
#         "{{-- <link href=\"{{ url('vendor/select2/dist/css/select2.min.css') }}\" rel=\"stylesheet\"> --}}",
#         "@endsection",
#         "",
#         "",
#         "@section('breadcrumbs')",
#         "<li class=\"breadcrumb-item\"><a href=\"{{ route('instruments') }}\">Instrumente</a></li>",
#         "<li class=\"breadcrumb-item active\">{{ $instrumentName. ' - Sectiunea '.$sectionNumber }}</li>",
#         "@endsection",
#         "",
#         "@section('content')",
#         "<div class=\"row\">",
#         "    <div class=\"col-md-12\">",
#         "        <div class=\"card border-secondary mb-3\">",
#         "            <div class=\"card-body\">",
#         "                @include('instruments.instrument-buttons')",
#         "                "
#     )

#     footer <- c(
#         "",
#         "",
#         "            </div>",
#         "        </div>",
#         "    </div>",
#         "</div>",
#         "@include('instruments.instrument-forms')",
#         "@endsection",
#         "",
#         "@push('SScripts')",
#         "    @include('instruments.instrument-scripts')",
#         "@endpush",
#         ""
#     )


#     html <- readLines(html)
#     sections <- which(grepl("--- SECTIUNE _", html))

#     for (i in seq(length(sections))) {
#         if (i < length(sections)) {
#             section <- unlist(strsplit(html[sections[i]], split = "_"))[2]
#             writeLines(
#                 c(
#                     header,
#                     paste("        ", html[seq(sections[i] + 1, sections[i + 1] - 1)], sep = ""),
#                     footer
#                 ),
#                 file.path(dir, sprintf("section_%s.blade.php", section))
#             )
#         }
#     }

}
