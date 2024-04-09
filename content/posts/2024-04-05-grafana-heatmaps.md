---
date: 2024-04-05T11:02:05+10:00
title: Grafana Heatmaps
author: "Carlo Hamalainen"
url: /2024/04/05/grafana-heatmaps
---

A service that does any important kind of work will have some critical sections, for example looking up
data in a cache, computing an intermediate result, and so on. My favourite visualisation for such durations
is the [heatmap](https://grafana.com/docs/grafana/latest/panels-visualizations/visualizations/heatmap/)
in [Grafana](https://grafana.com):

{{< figure src="/heatmap_nice.png" width=70% >}}

A heatmap is a time series visualisation where each time slice is a histogram. The colour of a cell is the count of the number
of durations that fall into that bucket. Unlike other visualisation methods like time series averages or
quantiles, the entire distribution of values is shown. Outliers will always be apparent. It's important to track
outliers because they correspond to our users' worst experiences - think of them as [misery metrics](https://www.p99conf.io/session/misery-metrics-consequences/).

Using our heatmap we can make qualitative statements:

* Periodically there is a burst of 10 or so requests that result in 1500ms processing times.
* Typical responses are around 1000ms. There was a bucket of 17 (white cell) show this is the most frequent processing time.
* It is rare for a request to be processed in under 1000ms. When it happens there is only a single request (dark cells corresponding to single values).

I use heatmaps extensively in my day job. There, other patterns immediately show up like the Mon-Fri/Sat trading week, Singapore
opening hours, and periodic slowdown due to systems being impact by other large batches or services.

# Logging durations

A simple way to capture these metrics is to write a log line for each request with the duration in milliseconds:

```json
{
  "time": "2024-04-05T10:59:25.850449+10:00",
  "level": "INFO",
  "source": {
    "function": "main.main",
    "file": "/Users/carlo/grafana/demo-logs/main.go",
    "line": 25
  },
  "msg": "step",
  "version": 1,
  "duration": 860
}
```

Using slog in Go, we produce lines like so:

```go
    logger.Info("step", "duration", rand.Intn(1000))  // synthetic data for the blog post
```

(In reality we would take note of the walltime before and after and compute the duration. Or go one better and use OpenTelemetry to trace the process.)

We use [Promtail](https://grafana.com/docs/loki/latest/send-data/promtail/) to ship the
logs to [Loki](https://grafana.com/docs/loki/latest/). Querying
using [logcli](https://grafana.com/docs/loki/latest/query/logcli/), we filter on ``job="app"`` and select lines with a ``duration``:

```shell-session
$ LOKI_ADDR=http://localhost:3100 logcli query '{job="app"} |= `duration`'
2024/04/05 10:59:49 http://localhost:3100/loki/api/v1/query_range?direction=BACKWARD&end=1712278789325735000&limit=30&query=%7Bjob%3D%22app%22%7D+%7C%3D+%60duration%60&start=1712275189325735000
2024/04/05 10:59:49 Common labels: {filename="/tmp/app.log", job="app", service_name="app"}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800617+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":945}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800607+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":782}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800597+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":11}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800587+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":667}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800577+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":236}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800566+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":303}
2024-04-05T10:59:26+10:00 {} {"time":"2024-04-05T10:59:26.800554+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":175}
```

Looks good, but we want to extract the ``duration``, so pipe it to the ``json`` stage so that it appears as a label:

```shell-session
$ LOKI_ADDR=http://localhost:3100 logcli query '{job="app"} |= `duration` | json'
2024/04/05 11:04:23 http://localhost:3100/loki/api/v1/query_range?direction=BACKWARD&end=1712279063986246000&limit=30&query=%7Bjob%3D%22app%22%7D+%7C%3D+%60duration%60+%7C+json&start=1712275463986246000
2024/04/05 11:04:23 Common labels: {filename="/tmp/app.log", job="app", level="INFO", msg="step", service_name="app", source_file="/Users/carlo/grafana/demo-logs/main.go", source_function="main.main", source_line="25", version="1"}
2024-04-05T10:59:26+10:00 {duration="945", time="2024-04-05T10:59:26.800617+10:00"} {"time":"2024-04-05T10:59:26.800617+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":945}
2024-04-05T10:59:26+10:00 {duration="782", time="2024-04-05T10:59:26.800607+10:00"} {"time":"2024-04-05T10:59:26.800607+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":782}
2024-04-05T10:59:26+10:00 {duration="11", time="2024-04-05T10:59:26.800597+10:00"}  {"time":"2024-04-05T10:59:26.800597+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":11}
2024-04-05T10:59:26+10:00 {duration="667", time="2024-04-05T10:59:26.800587+10:00"} {"time":"2024-04-05T10:59:26.800587+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":667}
2024-04-05T10:59:26+10:00 {duration="236", time="2024-04-05T10:59:26.800577+10:00"} {"time":"2024-04-05T10:59:26.800577+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":236}
2024-04-05T10:59:26+10:00 {duration="303", time="2024-04-05T10:59:26.800566+10:00"} {"time":"2024-04-05T10:59:26.800566+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":303}
2024-04-05T10:59:26+10:00 {duration="175", time="2024-04-05T10:59:26.800554+10:00"} {"time":"2024-04-05T10:59:26.800554+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":175}
2024-04-05T10:59:26+10:00 {duration="926", time="2024-04-05T10:59:26.800538+10:00"} {"time":"2024-04-05T10:59:26.800538+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":926}
2024-04-05T10:59:26+10:00 {duration="613", time="2024-04-05T10:59:26.800482+10:00"} {"time":"2024-04-05T10:59:26.800482+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":613}
2024-04-05T10:59:26+10:00 {duration="173", time="2024-04-05T10:59:26.800206+10:00"} {"time":"2024-04-05T10:59:26.800206+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":173}
2024-04-05T10:59:25+10:00 {duration="37", time="2024-04-05T10:59:25.85054+10:00"}   {"time":"2024-04-05T10:59:25.85054+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":37}
2024-04-05T10:59:25+10:00 {duration="145", time="2024-04-05T10:59:25.850523+10:00"} {"time":"2024-04-05T10:59:25.850523+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":145}
2024-04-05T10:59:25+10:00 {duration="473", time="2024-04-05T10:59:25.850506+10:00"} {"time":"2024-04-05T10:59:25.850506+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":473}
2024-04-05T10:59:25+10:00 {duration="452", time="2024-04-05T10:59:25.850491+10:00"} {"time":"2024-04-05T10:59:25.850491+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":452}
2024-04-05T10:59:25+10:00 {duration="881", time="2024-04-05T10:59:25.850479+10:00"} {"time":"2024-04-05T10:59:25.850479+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":881}
2024-04-05T10:59:25+10:00 {duration="418", time="2024-04-05T10:59:25.850464+10:00"} {"time":"2024-04-05T10:59:25.850464+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":418}
2024-04-05T10:59:25+10:00 {duration="860", time="2024-04-05T10:59:25.850449+10:00"} {"time":"2024-04-05T10:59:25.850449+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":860}
```

Somewhat surprisingly the labels are all strings

```
{duration="860", time="2024-04-05T10:59:25.850449+10:00"}
```

but duration is definitely a number in our original logs.

Reading [loki/latest/query/template_functions](https://grafana.com/docs/loki/latest/query/template_functions/) we might be tempted to use a
template to turn the string into an integer. Let's create a new label ``duration2`` and use a template:

```
{job="app"} |= `duration`
            | json
            | label_format duration2=`{{ .duration | int }}`
```

Still no luck; ``duration2`` is a string:

```shell-session
$ LOKI_ADDR=http://localhost:3100 logcli query '{job="app"} |= `duration` | json | label_format duration2=`{{ .duration | int }}`'
2024/04/05 11:06:56 http://localhost:3100/loki/api/v1/query_range?direction=BACKWARD&end=1712279216041691000&limit=30&query=%7Bjob%3D%22app%22%7D+%7C%3D+%60duration%60+%7C+json+%7C+label_format+duration2%3D%60%7B%7B+.duration+%7C+int+%7D%7D%60&start=1712275616041691000
2024/04/05 11:06:56 Common labels: {filename="/tmp/app.log", job="app", level="INFO", msg="step", service_name="app", source_file="/Users/carlo/grafana/demo-logs/main.go", source_function="main.main", source_line="25", version="1"}

...

2024-04-05T10:59:11+10:00 {duration="141", duration2="141", time="2024-04-05T10:59:11.744378+10:00"} {"time":"2024-04-05T10:59:11.744378+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":141}
```

Wondering if the label formatter is working, let's multiply by 10:

```shell-session
$ LOKI_ADDR=http://localhost:3100 \
  logcli query '{job="app"} |= `duration` | json | label_format durationX10=`{{ mul .duration 10 }}`'
2024/04/05 11:32:56 http://localhost:3100/loki/api/v1/query_range?direction=BACKWARD&end=1712280776533444000&limit=30&query=%7Bjob%3D%22app%22%7D+%7C%3D+%60duration%60+%7C+json+%7C+label_format+durationX10%3D%60%7B%7B+mul+.duration+10+%7D%7D%60&start=1712277176533444000
2024/04/05 11:32:56 Common labels: {filename="/tmp/app.log", job="app", level="INFO", msg="step", service_name="app", source_file="/Users/carlo/grafana/demo-logs/main.go", source_function="main.main", source_line="25", version="1"}

...

2024-04-05T10:59:11+10:00 {duration="141", durationX10="1410", time="2024-04-05T10:59:11.744378+10:00"} {"time":"2024-04-05T10:59:11.744378+10:00","level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":25},"msg":"step","version":1,"duration":141}
```

It is working: we see ``{duration="141", durationX10="1410", ..."}``.

Looking into the [source for Loki](https://github.com/grafana/loki/blob/3ece2ea6c470b7a53e92c395e28af2999c328199/pkg/loghttp/query.go#L277-L281) we see that ``Stream`` is a ``LabelSet``, and ``LabelSet`` is a ``map[string]string``:

```go
type Stream struct {
	Labels  LabelSet `json:"stream"`
	Entries []Entry  `json:"values"`
}

// LabelSet is a key/value pair mapping of labels
type LabelSet map[string]string
```

It was easy to find the ``LabelSet`` in the response by setting a few breakpoints, attaching to a running ``loki`` process, and
sending a query using ``logcli``.

{{< figure src="/debugging_loki_stream.jpg" width=70% >}}

There are two ways around this issue of labels being strings.

## "no heatmap fields found"

Given that labels are strings, let's go a step back and try to create a heatmap in Grafana using the
basic query

```
{job="app"} |= `duration` | json
```

We expect this to fail because there are no numerical fields:

{{< figure src="/no_heatmap_fields.png" width=70% >}}

Let's add two transformations: first extract the labels, and then convert ``duration`` to a number:

{{< figure src="/transforms-grafana.png" width=50% >}}

This gives us our nice heatmap:

{{< figure src="/heatmap_nice.png" width=70% >}}

Side note: I asked Grafana to calculate the heatmap from my data (Heatmap settings, ``Calculate from data = yes``).

Also beware that the Loki query has an implicit line limit - here I set it to the maximum 5000:

{{< figure src="/line_limit.png" width=60% >}}

At scale, with tens of thousands of requests in the viewing range,
the heatmap would not be truly representative of the data because
some log entries would be dropped.

## Using max_over_time instead

A better idea is to use a [metric query](https://grafana.com/docs/loki/latest/query/metric_queries/#unwrapped-range-aggregations)
so that Loki can aggregate data server-side.

In our case, we care about the worst possible behaviour, so we use a maximum over time:

```
max_over_time({job="app"} |= `duration`
                          | json
                          | unwrap duration [$__auto])
```

This is simpler to use in Grafana as well - we don't have to set up the two transformations to extract ``duration`` as a number.

## Quirks of heatmaps with custom ranges

I noticed that LogQL/Grafana can oversample when we set the range to a fixed value and zoom in too far (here we use ``[1m]`` for one minute).

{{< figure src="/max_over_time_overcount.png" width=70% >}}

Grafana drew 20 squares representing the count 50, and one for count 45; This total 1045 corresponds to the number of times that ``1899`` is sampled. But in this contrived example there is only a single log entry with ``duration = 1899``:

```
{"time":"2024-04-06T18:36:57.80441+10:00","duration":933,"level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":26},"msg":"step","version":1}
{"time":"2024-04-06T18:47:44.567945+10:00","duration":1042,"level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":26},"msg":"step","version":1}
{"time":"2024-04-06T19:05:14.911419+10:00","duration":1501,"level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":26},"msg":"step","version":1}
{"time":"2024-04-06T19:16:23.747189+10:00","duration":1522,"level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":26},"msg":"step","version":1}
{"time":"2024-04-06T19:17:56.945765+10:00","duration":1899,"level":"INFO","source":{"function":"main.main","file":"/Users/carlo/grafana/demo-logs/main.go","line":26},"msg":"step","version":1}
```

Adding a ``fmt.Printf`` to [engine.go](https://github.com/grafana/loki/blob/45ca2fa5122e3281c4dbd107977cc655a43574d8/pkg/logql/engine.go#L437-L444):

```go
series := make([]promql.Series, 0, len(seriesIndex))
	for _, s := range seriesIndex {
		series = append(series, *s)
		fmt.Printf("appending series: %v\n", *s) // debugging
	}
	result := promql.Matrix(series)
	sort.Sort(result)

	return result, stepEvaluator.Error()
```

shows us where the oversampling is happening:

```
appending series: {filename="/tmp/app.log", job="app", level="INFO", msg="step", service_name="app", source_file="/Users/carlo/grafana/demo-logs/main.go", source_function="main.main", source_line="26", time="2024-04-06T19:17:56.945765+10:00", version="1"} =>
1899 @[1712395108000]
1899 @[1712395108001]
1899 @[1712395108002]
1899 @[1712395108003]
1899 @[1712395108004]
1899 @[1712395108005]
1899 @[1712395108006]
1899 @[1712395108007]
1899 @[1712395108008]
1899 @[1712395108009]
1899 @[1712395108010]
1899 @[1712395108011]
1899 @[1712395108012]
1899 @[1712395108013]
1899 @[1712395108014]
1899 @[1712395108015]
(Total 1045 lines of this)
```

My understanding is that the "step size" is too small relative to the total time period so the sliding window samples the single
value 1899 too many times.

The heatmap works fine when we set the range to the default ``[$__auto]`` in Grafana.

## Further reading

How to undersample: <https://www.robustperception.io/step-and-query_range/>

Loki builds on Prometheus as one can see from the code snippet above: the series are ``promql.Series``. This blog post explains how
the range aggregations work: <https://iximiuz.com/en/posts/prometheus-functions-agg-over-time/>

An old Github issue about step, resolution, interval, etc:
[Loki: step/resolution/interval/min_interval is confusing #52387](https://github.com/grafana/grafana/issues/52387)

An old Github issue about interval not scaling with resolution:
[ $__interval doesn't scale with Loki query resolution value #52290 ](https://github.com/grafana/grafana/issues/52290)

A talk about "misery metrics" from p99conf:

{{< youtube K1jasTyGLr8 >}}