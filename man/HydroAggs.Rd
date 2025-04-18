% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HydroAggsFactory.R
\name{HydroAggs}
\alias{HydroAggs}
\alias{HydroAggsFactory}
\title{R6 object for imported data via the EA API}
\value{
A class containing raw data, metadata and methods
}
\description{
This is the base class for all data imported via the API. A HydroAggs
contains public raw data and private metadata.
}
\section{Super class}{
\code{\link[riskyData:HydroImport]{riskyData::HydroImport}} -> \code{HydroAggs}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-HydroAggs-print}{\code{HydroAggsFactory$print()}}
\item \href{#method-HydroAggs-clone}{\code{HydroAggsFactory$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="addCatchProp"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-addCatchProp'><code>riskyData::HydroImport$addCatchProp()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="addRating"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-addRating'><code>riskyData::HydroImport$addRating()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="asVol"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-asVol'><code>riskyData::HydroImport$asVol()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="coords"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-coords'><code>riskyData::HydroImport$coords()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="dataAgg"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-dataAgg'><code>riskyData::HydroImport$dataAgg()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="dayStats"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-dayStats'><code>riskyData::HydroImport$dayStats()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="exceed"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-exceed'><code>riskyData::HydroImport$exceed()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="findPeaks"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-findPeaks'><code>riskyData::HydroImport$findPeaks()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="flowDuration"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-flowDuration'><code>riskyData::HydroImport$flowDuration()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="hydroYearDay"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-hydroYearDay'><code>riskyData::HydroImport$hydroYearDay()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="initialize"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-initialize'><code>riskyData::HydroImport$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="meta"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-meta'><code>riskyData::HydroImport$meta()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="methods"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-methods'><code>riskyData::HydroImport$methods()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="missing"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-missing'><code>riskyData::HydroImport$missing()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="nrfa"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-nrfa'><code>riskyData::HydroImport$nrfa()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="plot"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-plot'><code>riskyData::HydroImport$plot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="postOrder"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-postOrder'><code>riskyData::HydroImport$postOrder()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="quality"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-quality'><code>riskyData::HydroImport$quality()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rateFlow"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rateFlow'><code>riskyData::HydroImport$rateFlow()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rateStage"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rateStage'><code>riskyData::HydroImport$rateStage()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rmHY"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rmHY'><code>riskyData::HydroImport$rmHY()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rmHYD"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rmHYD'><code>riskyData::HydroImport$rmHYD()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rmVol"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rmVol'><code>riskyData::HydroImport$rmVol()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="rollingAggs"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-rollingAggs'><code>riskyData::HydroImport$rollingAggs()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="statSummary"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-statSummary'><code>riskyData::HydroImport$statSummary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="summary"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-summary'><code>riskyData::HydroImport$summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="riskyData" data-topic="HydroImport" data-id="window"><a href='../../riskyData/html/HydroImport.html#method-HydroImport-window'><code>riskyData::HydroImport$window()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HydroAggs-print"></a>}}
\if{latex}{\out{\hypertarget{method-HydroAggs-print}{}}}
\subsection{Method \code{print()}}{
Display the R6 object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HydroAggsFactory$print(.)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{.}}{(ignored).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HydroAggs-clone"></a>}}
\if{latex}{\out{\hypertarget{method-HydroAggs-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HydroAggsFactory$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
