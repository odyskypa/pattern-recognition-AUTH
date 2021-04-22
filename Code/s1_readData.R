# Find the files in the current working directory
filenames = list.files()
methodFilenames = filenames[endsWith(filenames, "MethodMetrics.csv")]
violationFilenames = filenames[endsWith(filenames, "PMDViolations.csv")]

total_metrics = data.frame()
i = 0
for (file in methodFilenames) {
  i = i + 1
  print(i)
  metrics = read.csv(file, sep=";", header = TRUE, na.strings = c("NA"))
  total_metrics = rbind(total_metrics, metrics)
}

total_violations = data.frame()
i = 0
for (file in violationFilenames) {
  i = i + 1
  print(i)
  violations = read.csv(file, sep=";", header = TRUE, na.strings = c("NA"))
  total_violations = rbind(total_violations, violations)
}