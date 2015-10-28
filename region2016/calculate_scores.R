# load required libraries
suppressWarnings(require(ohicore))

# set working directory to the scenario directory, ie containing conf and layers directories
setwd('~/github/ohibc/region2016')

# load scenario configuration
conf = Conf('conf')

# run checks on scenario layers
CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load scenario layers
layers = Layers('layers.csv', 'layers')

# calculate scenario scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)

# merge to published branch (to display on app). Make sure all local work is committed.
merge_branches = F #change to T, or run line 33-41.

if (merge_branches) {
  # switch to draft branch and get latest
  system('git checkout draft; git pull')

  # merge published with the draft branch
  system('git checkout published')
  system('git merge draft')
  system('git push origin published')

  # switch to draft branch and get latest
  system('git checkout draft; git pull')
}


