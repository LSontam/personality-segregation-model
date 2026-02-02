;; =================================================================
;; PERSONALITY-BASED SEGREGATION MODEL (Patch-Based)
;; Inspired by Schelling's model but using Big Five personality
;; =================================================================

globals [
  percent-incompatible
  percent-unhappy
  chol-matrix
  psychDist-summary
;  use-local-search ;; true for Schelling's logic, false for global teleportation
;  use-dualLevel-calc-A_N
;  use-extreme-incompatibility-rule
]

turtles-own [
  openness_T
  conscientiousness_T
  extraversion_T
  agreeableness_T
  neuroticism_T
  openness
  conscientiousness
  extraversion
  agreeableness
  neuroticism
  compatible-nearby
  total-nearby
  incompatible-nearby
  my-pairwise-incompatibility-threshold
  my-%-compatible-neighbors-wanted
  happy?
  cluster-id
  sub-cluster-id  ;; secondary ID for density-based sub-clustering
]

to setup
  clear-all

  ;; Precomputed Cholesky decomposition
  set chol-matrix [
    [ 1.0000  0      0      0      0     ]
    [ 0.2000  0.9800 0      0      0     ]
    [ 0.4300  0.2100 0.8800 0      0     ]
    [ 0.2100  0.3900 0.1800 0.8800 0     ]
    [-0.1700 -0.3700 -0.3000 -0.2800 0.8900 ]
  ]

  ;; CHANGE 1: Sprout turtles on patches (like Schelling)
  ;; Note: 'number' is your slider for population size
  ask n-of number patches [
    sprout 1 [
      assign-personality
      set my-%-compatible-neighbors-wanted calculate-compatible-wanted
      set-trait-color
      set my-pairwise-incompatibility-threshold calculate-incompatibility-threshold
      set happy? false  ;; Start everyone unhappy so they begin moving
      set compatible-nearby 0
      set incompatible-nearby 0
      set total-nearby 0
    ]
  ]

  ;; Don't call update-turtles here - let go procedure do it
  reset-ticks
  update-globals

end

;; ========================================
;; PERSONALITY ASSIGNMENT (unchanged)
;; ========================================

to assign-personality
  let z (list (random-normal 0 1)
    (random-normal 0 1)
    (random-normal 0 1)
    (random-normal 0 1)
    (random-normal 0 1))

  let correlated (matrixx chol-matrix z)
  let t-scores map [ val -> 50 + (10 * val) ] correlated

  set openness_T item 0 t-scores
  set conscientiousness_T item 1 t-scores
  set extraversion_T item 2 t-scores
  set agreeableness_T item 3 t-scores
  set neuroticism_T item 4 t-scores

  set openness clamp-value (openness_T)
  set conscientiousness clamp-value (conscientiousness_T)
  set extraversion clamp-value (extraversion_T)
  set agreeableness clamp-value (agreeableness_T)
  set neuroticism clamp-value (neuroticism_T)
end

to-report matrixx [mat vec]
  let result []
  foreach mat [
    row ->
      let summ 0
      (foreach row vec [
        [a b] -> set summ (summ + (a * b))
      ])
      set result lput summ result
  ]
  report result
end

to-report clamp-value [v]
  report max list 0 (min list 100 v)
end

to-report calculate-incompatibility-threshold
  let base-threshold pairwise-incompatibility-threshold
  let personality-adjustment (
    0.3 * (openness - 50) +
    0.3 * (agreeableness - 50) +
    -0.4 * (neuroticism - 50)
  )
  let final-threshold base-threshold + personality-adjustment

  ;; Proper clamping with explicit parentheses
  if final-threshold < 0 [ report 0 ]
  if final-threshold > 500 [ report 500 ]
  report final-threshold
end

to-report calculate-compatible-wanted
  let base-pct %-compatible-neighbors-wanted
  let personality-adjustment (
    -0.4 * (extraversion - 50) +     ; NEGATIVE - less demanding
    -0.3 * (agreeableness - 50) +    ; NEGATIVE - more tolerant
    0.4 * (neuroticism - 50)         ; POSITIVE - more demanding
  )
  let final-pct base-pct + personality-adjustment

  ;; Proper clamping
  if final-pct < 0 [ report 0 ]
  if final-pct > 100 [ report 100 ]
  report final-pct
end

to set-trait-color
  let trait-scores (list openness conscientiousness extraversion agreeableness neuroticism)
  let max-score max trait-scores
  let max-indices filter [i -> (item i trait-scores) = max-score] (range length trait-scores)
  let max-index one-of max-indices
  let trait-colors [red orange yellow green blue]
  set color item max-index trait-colors
end

;; ========================================
;; MAIN LOOP
;; ========================================

to go
  if all? turtles [happy?] [
    evaluate-psychDist-metrics ;; calculate final metrics at equilibrium
    stop
  ]

  move-unhappy-turtles
  update-turtles
  update-globals
  tick
end

to move-unhappy-turtles
  ask turtles with [ not happy? ] [
    find-new-spot
  ]
end

to find-new-spot
  ifelse use-local-search
  [find-new-spot-local]  ;; Schelling's logic
  [find-new-spot-global] ;; global movement logic
end


;; Schelling's movement logic
to find-new-spot-local
  rt random-float 360
  fd random-float 10
  if any? other turtles-here
    [ find-new-spot-local ]          ;; keep going until find an unoccupied patch
  setxy pxcor pycor  ;; move to center of patch
end

;; Global movement - teleport to an empty spot
to find-new-spot-global
  ;; Find an unoccupied patch to move to
  let target-patch one-of patches with [not any? turtles-here]

  if target-patch != nobody [
    move-to target-patch
  ]
  ;; If no empty patches, stay put (world is full)
end

;; ========================================
;; CHANGE 3: PATCH-BASED COMPATIBILITY EVALUATION
;; ========================================

to update-turtles
  ask turtles [
    let my-neighbors turtles-on neighbors

    set total-nearby count my-neighbors

;    if total-nearby = 0 [
;      set happy? true
;      set incompatible-nearby 0
;      set compatible-nearby 0
;      stop
;    ]
    if total-nearby = 0 [
  ;; Extraverts are unhappy with no neighbors, others are happy
  ifelse extraversion > 50 [
    set happy? false
  ] [
    set happy? true
  ]
  set incompatible-nearby 0
  set compatible-nearby 0
  stop
]

let incomp-count 0
let has-extreme-incompatibility false

ask my-neighbors [
  let dist calculate-psychDist-between myself self
  if dist > [my-pairwise-incompatibility-threshold] of myself [
    set incomp-count incomp-count + 1
  ]
  ;; Check if any neighbor has distance > 1.5 times threshold (only if rule enabled)
  if use-extreme-incompatibility-rule and dist > (1.5 * [my-pairwise-incompatibility-threshold] of myself) [
    set has-extreme-incompatibility true
  ]
]

set incompatible-nearby incomp-count
set compatible-nearby total-nearby - incompatible-nearby

let compatible-ratio (compatible-nearby / total-nearby)
ifelse use-extreme-incompatibility-rule [
  set happy? (compatible-ratio >= (my-%-compatible-neighbors-wanted / 100) and not has-extreme-incompatibility)
] [
  set happy? (compatible-ratio >= (my-%-compatible-neighbors-wanted / 100))
]
  ]
end

;; ========================================
;; PSYCHDIST CALCULATION (standalone reporter)
;; ========================================

to-report calculate-psychDist-between [agent1 agent2]
  let dist 0

  let a1-o [openness] of agent1
  let a1-c [conscientiousness] of agent1
  let a1-e [extraversion] of agent1
  let a1-a [agreeableness] of agent1
  let a1-n [neuroticism] of agent1

  let a2-o [openness] of agent2
  let a2-c [conscientiousness] of agent2
  let a2-e [extraversion] of agent2
  let a2-a [agreeableness] of agent2
  let a2-n [neuroticism] of agent2

  ;; 1. Openness High/Low
  if ((a1-o > 50 and a2-o < 50) or (a1-o < 50 and a2-o > 50)) [
    set dist dist + abs(a1-o - a2-o)
  ]

  ;; 2. Conscientiousness High/Low
  if ((a1-c > 50 and a2-c < 50) or (a1-c < 50 and a2-c > 50)) [
    set dist dist + abs(a1-c - a2-c)
  ]

  ;; 3. Extraversion High/Low
  if ((a1-e > 50 and a2-e < 50) or (a1-e < 50 and a2-e > 50)) [
    set dist dist + abs(a1-e - a2-e)
  ]


  ;; 4. Agreeableness Low/Low
  ifelse use-dualLevel-calc-A_N
   [;; Dual-Level Calculation - Consider both extremity and dissimilarity

    if (a1-a < 50 and a2-a < 50) [
      let avg-deviation (abs(50 - a1-a) + abs(50 - a2-a)) / 2
      let dissimilarity abs(a1-a - a2-a)
      set dist dist + (1.5 * avg-deviation) + (dissimilarity * 0.5)
      ]
    ]
    [;; Old Calculation - Agreeableness Low/Low
      if (a1-a < 50 and a2-a < 50) [
        set dist dist + (100 - abs(a1-a - a2-a))
      ]
    ]
  ;; 5. Neuroticism High/Low
  if ((a1-n > 50 and a2-n < 50) or (a1-n < 50 and a2-n > 50)) [
    set dist dist + abs(a1-n - a2-n)
  ]


  ;; 6. Neuroticism High/High
  ifelse use-dualLevel-calc-A_N
    [;; Dual-level Calculation - Consider both extremity and dissimilarity
      if (a1-n > 50 and a2-n > 50) [
        let avg-deviation (abs(50 - a1-n) + abs(50 - a2-n)) / 2
        let dissimilarity abs(a1-n - a2-n)
        set dist dist + (1.5 * avg-deviation) + (dissimilarity * 0.5)
      ]
    ]
  [;; Old Calculation - Neuroticism High/High
    if (a1-n > 50 and a2-n > 50) [
      set dist dist + (100 - abs(a1-n - a2-n))
    ]
  ]

  report dist
end

to-report calculate-all-psychDist-values
  let all-distances []
  ask turtles[
    let my-neighbors turtles-on neighbors
    ask my-neighbors [
      let dist calculate-psychDist-between myself self
      set all-distances lput dist all-distances
    ]
  ]
  report all-distances
end

to  evaluate-psychDist-metrics
  let distances calculate-all-psychDist-values

  if length distances > 0 [
    let mean-psychDist mean distances
    let min-psychDist min distances
    let max-psychDist max distances
    set psychDist-summary (word "Mean: " precision mean-psychDist 2
                                 " | Min: " precision min-psychDist 2
                                 " | Max: " precision max-psychDist 2)

;    print psychDist-summary
  ]
end
;; ========================================
;; GLOBAL METRICS
;; ========================================

to update-globals
  let incompatible-neighbors sum [incompatible-nearby] of turtles
  let total-neighbors sum [total-nearby] of turtles

  if total-neighbors > 0 [
    set percent-incompatible (incompatible-neighbors / total-neighbors) * 100
  ]

  if count turtles > 0 [
    set percent-unhappy (count turtles with [not happy?]) / (count turtles) * 100
  ]

  ;; Only update psychDist metrics every N ticks
  if ticks mod 100 = 0 [
    evaluate-psychDist-metrics
  ]
end





to check-correlations-simple
  print "=========================================="
  print "TRAIT CORRELATIONS"
  print "=========================================="

  ;; Retrieve values in WHO order
  let O-vals []
  let C-vals []
  let E-vals []
  let A-vals []
  let N-vals []

  let i 0
  repeat count turtles [
    ask turtle i [
      set O-vals lput openness O-vals
      set C-vals lput conscientiousness C-vals
      set E-vals lput extraversion E-vals
      set A-vals lput agreeableness A-vals
      set N-vals lput neuroticism N-vals
    ]
    set i i + 1
  ]

  print (word "O-C: " precision (calculate-correlation O-vals C-vals) 3)
  print (word "O-E: " precision (calculate-correlation O-vals E-vals) 3)
  print (word "O-A: " precision (calculate-correlation O-vals A-vals) 3)
  print (word "O-N: " precision (calculate-correlation O-vals N-vals) 3)
  print (word "C-E: " precision (calculate-correlation C-vals E-vals) 3)
  print (word "C-A: " precision (calculate-correlation C-vals A-vals) 3)
  print (word "C-N: " precision (calculate-correlation C-vals N-vals) 3)
  print (word "E-A: " precision (calculate-correlation E-vals A-vals) 3)
  print (word "E-N: " precision (calculate-correlation E-vals N-vals) 3)
  print (word "A-N: " precision (calculate-correlation A-vals N-vals) 3)

    print ""
  print "TARGET CORRELATIONS:"
  print "O-C: 0.20"
  print "O-E: 0.43"
  print "O-A: 0.21"
  print "O-N: -0.17"
  print "C-E: 0.29"
  print "C-A: 0.43"
  print "C-N: -0.43"
  print "E-A: 0.26"
  print "E-N: -0.36"
  print "A-N: -0.36"

  print "=========================================="

  print "=========================================="
end

to-report calculate-correlation [list1 list2]
  if length list1 != length list2 or length list1 < 2 [
    report 0
  ]

  let mean1 mean list1
  let mean2 mean list2

  let numerator 0
  let sum-sq1 0
  let sum-sq2 0

  (foreach list1 list2 [ [x y] ->
    let dev1 (x - mean1)
    let dev2 (y - mean2)
    set numerator numerator + (dev1 * dev2)
    set sum-sq1 sum-sq1 + (dev1 * dev1)
    set sum-sq2 sum-sq2 + (dev2 * dev2)
  ])

  let denominator sqrt (sum-sq1 * sum-sq2)

  ifelse denominator = 0
    [ report 0 ]
    [ report numerator / denominator ]
end


to trait-and-color-summary
  print "=========================================="
  print "TRAIT STATISTICS"
  print "=========================================="

  print (word "Openness:           Mean=" precision (mean [openness] of turtles) 2
             " Min=" precision (min [openness] of turtles) 2
             " Max=" precision (max [openness] of turtles) 2)

  print (word "Conscientiousness:  Mean=" precision (mean [conscientiousness] of turtles) 2
             " Min=" precision (min [conscientiousness] of turtles) 2
             " Max=" precision (max [conscientiousness] of turtles) 2)

  print (word "Extraversion:       Mean=" precision (mean [extraversion] of turtles) 2
             " Min=" precision (min [extraversion] of turtles) 2
             " Max=" precision (max [extraversion] of turtles) 2)

  print (word "Agreeableness:      Mean=" precision (mean [agreeableness] of turtles) 2
             " Min=" precision (min [agreeableness] of turtles) 2
             " Max=" precision (max [agreeableness] of turtles) 2)

  print (word "Neuroticism:        Mean=" precision (mean [neuroticism] of turtles) 2
             " Min=" precision (min [neuroticism] of turtles) 2
             " Max=" precision (max [neuroticism] of turtles) 2)

  print ""
  print "=========================================="
  print "COLOR DISTRIBUTION"
  print "=========================================="

  let red-count count turtles with [color = red]
  let orange-count count turtles with [color = orange]
  let yellow-count count turtles with [color = yellow]
  let green-count count turtles with [color = green]
  let blue-count count turtles with [color = blue]
  let total count turtles

  print (word "Red (Openness):           " red-count " (" precision (red-count / total * 100) 1 "%)")
  print (word "Orange (Conscientiousness): " orange-count " (" precision (orange-count / total * 100) 1 "%)")
  print (word "Yellow (Extraversion):    " yellow-count " (" precision (yellow-count / total * 100) 1 "%)")
  print (word "Green (Agreeableness):    " green-count " (" precision (green-count / total * 100) 1 "%)")
  print (word "Blue (Neuroticism):       " blue-count " (" precision (blue-count / total * 100) 1 "%)")

  print "=========================================="
end



to identify-spatial-clusters
  ;; Reset all cluster IDs
  ask turtles [
    set cluster-id -1
  ]

  let current-id 0

  ;; For each unassigned turtle, start a new cluster
  ask turtles [
    if cluster-id = -1 [
      flood-fill-cluster current-id self
      set current-id current-id + 1
    ]
  ]
end

to flood-fill-cluster [id starting-turtle]
  ;; Mark the starting turtle
  ask starting-turtle [
    if cluster-id = -1 [
      set cluster-id id

      ;; Get all neighbors and recursively assign them
      ask turtles-on neighbors [
        if cluster-id = -1 [
          flood-fill-cluster id self
        ]
      ]
    ]
  ]
end

to color-clusters
  ask turtles [
    let cluster-colors [red orange yellow green blue magenta cyan white brown pink]
    set color item (cluster-id mod length cluster-colors) cluster-colors
  ]
end

to highlight-cluster [target-cluster-id]
  ;; Make all turtles gray first
  ask turtles [
    set color gray
  ]

  ;; Highlight the target cluster in bright red
  ask turtles with [cluster-id = target-cluster-id] [
    set color red
  ]

  print (word "Cluster " target-cluster-id " highlighted in red. (" (count turtles with [cluster-id = target-cluster-id]) " agents)")
end

;; ========================================
;; DENSITY-BASED SUB-CLUSTERING PROCEDURES
;; ========================================

to-report count-shared-neighbors [agent1 agent2 parent-cluster-id]
  ;; Count how many neighbors agent1 and agent2 have in common within the same parent cluster
  let neighbors1 turtles-on [neighbors] of agent1 with [cluster-id = parent-cluster-id and sub-cluster-id != -1]
  let neighbors2 turtles-on [neighbors] of agent2 with [cluster-id = parent-cluster-id and sub-cluster-id != -1]

  let shared-count 0
  ask neighbors1 [
    if member? self neighbors2 [
      set shared-count shared-count + 1
    ]
  ]
  report shared-count
end

to density-flood-fill-sub-cluster [sub-id starting-turtle parent-cluster-id]
  ;; Queue-based flood fill with adaptive density constraint
  ;; Initial recruitment is permissive (threshold=1), then strict (threshold=min-shared-neighbors) once established
  let frontier (list starting-turtle)
  let processed (list)

  ask starting-turtle [
    if sub-cluster-id = -1 and cluster-id = parent-cluster-id [
      set sub-cluster-id sub-id
    ]
  ]

  while [length frontier > 0] [
    let current-agent first frontier
    set frontier but-first frontier
    set processed lput current-agent processed

    ask current-agent [
      let unassigned-neighbors (turtles-on neighbors) with [cluster-id = parent-cluster-id and sub-cluster-id = -1]

      ask unassigned-neighbors [
        ;; Count how many neighbors of this agent are already in the sub-cluster
        let in-sub-cluster-count count (turtles-on neighbors) with [cluster-id = parent-cluster-id and sub-cluster-id = sub-id]

        ;; Adaptive threshold: permissive when bootstrapping, strict when established
        let current-sub-size count turtles with [sub-cluster-id = sub-id and cluster-id = parent-cluster-id]
        let threshold ifelse-value (current-sub-size < 4) [1] [min-shared-neighbors]

        if in-sub-cluster-count >= threshold [
          ;; Recruit this agent to the sub-cluster
          set sub-cluster-id sub-id

          ;; Add to frontier if not already processed
          if not member? self processed [
            set frontier lput self frontier
          ]
        ]
      ]
    ]
  ]
end

to identify-sub-clusters
  ;; Set default parameters from sliders if they exist (for robustness)
  if min-shared-neighbors = 0 [ set min-shared-neighbors 2 ]
  if min-sub-cluster-size = 0 [ set min-sub-cluster-size 3 ]

  ;; Reset all sub-cluster IDs
  ask turtles [
    set sub-cluster-id -1
  ]

  ;; For each original cluster, apply density-based subdivision
  let cluster-ids remove-duplicates [cluster-id] of turtles with [cluster-id != -1]

  foreach sort cluster-ids [cluster-id-val ->
    identify-sub-clusters-in-cluster cluster-id-val
  ]

  let total-sub-clusters length remove-duplicates [sub-cluster-id] of turtles with [sub-cluster-id != -1]
  let isolated-count count turtles with [sub-cluster-id = -1 and cluster-id != -1]

  print (word "Sub-clustering complete: " total-sub-clusters " sub-clusters identified, " isolated-count " isolated/chain agents")
end

to identify-sub-clusters-in-cluster [cluster-id-val]
  ;; Helper procedure for subdividing a single cluster
  let cluster-members turtles with [cluster-id = cluster-id-val]

  ;; Reset just this cluster's sub-cluster IDs
  ask cluster-members [
    set sub-cluster-id -1
  ]

  ;; Process each unassigned agent in this cluster
  let unassigned-members cluster-members with [sub-cluster-id = -1]
  let current-sub-id (cluster-id-val * 1000)

  print (word "Processing cluster " cluster-id-val " with " count cluster-members " members")

  repeat count unassigned-members [
    ;; Get one unassigned member to start a new sub-cluster
    let seed-agent one-of turtles with [cluster-id = cluster-id-val and sub-cluster-id = -1]

    if seed-agent != nobody [
      ;; Start density-based flood fill
      ask seed-agent [
        density-flood-fill-sub-cluster current-sub-id self cluster-id-val
      ]

      ;; Check if this sub-cluster meets minimum size
      let sub-cluster-size count turtles with [sub-cluster-id = current-sub-id and cluster-id = cluster-id-val]

      print (word "  Sub-cluster " cluster-id-val "." (current-sub-id mod 1000) ": " sub-cluster-size " agents (min required: " min-sub-cluster-size ")")

      ifelse sub-cluster-size >= min-sub-cluster-size [
        set current-sub-id current-sub-id + 1
      ] [
        ;; If sub-cluster is too small, revert to unclustered (-1)
        ask turtles with [sub-cluster-id = current-sub-id and cluster-id = cluster-id-val] [
          set sub-cluster-id -1
        ]
      ]
    ]
  ]
end

to color-sub-clusters
  ;; Color each sub-cluster with a distinct color, isolated agents in gray
  let sub-cluster-ids remove-duplicates [sub-cluster-id] of turtles with [sub-cluster-id != -1]
  let cluster-colors [red orange yellow green blue magenta cyan white brown pink]

  ask turtles with [sub-cluster-id = -1] [
    set color gray
  ]

  foreach sort sub-cluster-ids [sub-id ->
    ask turtles with [sub-cluster-id = sub-id] [
      set color item (sub-id mod length cluster-colors) cluster-colors
    ]
  ]
end

to color-sub-clusters-hierarchical
  ;; Color sub-clusters with shade variations of their parent cluster color
  let base-cluster-ids remove-duplicates [cluster-id] of turtles with [cluster-id != -1]

  ;; Mark isolated agents in gray
  ask turtles with [sub-cluster-id = -1] [
    set color gray
  ]

  ;; For each original cluster, assign shade variations to its sub-clusters
  foreach sort base-cluster-ids [base-cluster-val ->
    let cluster-members turtles with [cluster-id = base-cluster-val and sub-cluster-id != -1]

    ;; Skip clusters with no sub-clustered members (all isolated)
    if count cluster-members > 0 [
      let sub-ids-in-cluster remove-duplicates [sub-cluster-id] of cluster-members
      let base-color one-of [color] of cluster-members

      ;; Assign colors based on sub-cluster ID with shading
      let color-offset 0
      foreach sort sub-ids-in-cluster [sub-id ->
        ;; Create shade variation: base-color ± (1,2,3,4...)
        let adjusted-color base-color
        if color-offset > 0 [
          let variation-magnitude ((color-offset + 1) / 2)  ;; 0.5, 1, 1.5, 2...
          let variation-direction (ifelse-value (color-offset mod 2 = 0) [1] [-1])
          set adjusted-color base-color + (variation-direction * variation-magnitude)
        ]

        ask turtles with [sub-cluster-id = sub-id and cluster-id = base-cluster-val] [
          set color adjusted-color
        ]

        set color-offset color-offset + 1
      ]
    ]
  ]
end

to analyze-sub-clusters
  print "=========================================="
  print "SUB-CLUSTER ANALYSIS (Density-Based)"
  print "=========================================="
  print ""

  let total-sub-clusters length remove-duplicates [sub-cluster-id] of turtles with [sub-cluster-id != -1]
  let isolated-count count turtles with [sub-cluster-id = -1]
  let total-agents count turtles

  print (word "Total Sub-Clusters: " total-sub-clusters)
  print (word "Isolated/Chain Agents: " isolated-count " (" precision (isolated-count / total-agents * 100) 1 "%)")
  print ""

  ;; Analyze each original cluster and its sub-clusters
  let base-cluster-ids remove-duplicates [cluster-id] of turtles with [cluster-id != -1]

  foreach sort base-cluster-ids [base-id ->
    let base-members turtles with [cluster-id = base-id]
    let base-size count base-members
    let sub-ids-in-base remove-duplicates [sub-cluster-id] of base-members with [sub-cluster-id != -1]
    let sub-count length sub-ids-in-base
    let isolated-in-base count base-members with [sub-cluster-id = -1]

    print (word "ORIGINAL CLUSTER " base-id " (" base-size " agents)")
    print (word "  Sub-Clusters: " sub-count ", Isolated: " isolated-in-base)

    ;; Analyze each sub-cluster in this base cluster
    foreach sort sub-ids-in-base [sub-id ->
      analyze-single-sub-cluster sub-id base-id
    ]

    print ""
  ]

  ;; Compare within vs between sub-cluster psychDist
  compare-sub-cluster-psychDist

  print "=========================================="
end

to analyze-single-sub-cluster [sub-id base-cluster-id]
  let sub-members turtles with [sub-cluster-id = sub-id]
  let sub-size count sub-members

  if sub-size < 4 [ stop ]  ;; Only analyze sub-clusters with 4+ agents

  let display-id (word base-cluster-id "." (sub-id mod 1000))
  print (word "  Sub-Cluster " display-id " (" sub-size " agents):")

  ;; Mean trait scores
  let mean-o mean [openness] of sub-members
  let mean-c mean [conscientiousness] of sub-members
  let mean-e mean [extraversion] of sub-members
  let mean-a mean [agreeableness] of sub-members
  let mean-n mean [neuroticism] of sub-members

  print (word "    Mean Traits: O=" precision mean-o 1 " C=" precision mean-c 1 " E=" precision mean-e 1 " A=" precision mean-a 1 " N=" precision mean-n 1)

  ;; Within sub-cluster psychDist
  let within-distances []
  ask sub-members [
    let my-neighbors (turtles-on neighbors) with [sub-cluster-id = sub-id]
    ask my-neighbors [
      let dist calculate-psychDist-between myself self
      set within-distances lput dist within-distances
    ]
  ]

  if length within-distances > 0 [
    let mean-within mean within-distances
    print (word "    Within psychDist: " precision mean-within 2)
  ]

  print ""
end

to compare-sub-cluster-psychDist
  print "PSYCHDIST COMPARISON: Within vs Between Sub-Clusters"
  print "=========================================="

  ;; Within sub-cluster distances
  let within-sub-distances []
  ask turtles with [sub-cluster-id != -1] [
    let my-sub-id sub-cluster-id
    let same-sub-neighbors (turtles-on neighbors) with [sub-cluster-id = my-sub-id]
    ask same-sub-neighbors [
      let dist calculate-psychDist-between myself self
      set within-sub-distances lput dist within-sub-distances
    ]
  ]

  ;; Between sub-cluster distances (same parent cluster)
  let between-same-parent-distances []
  ask turtles with [sub-cluster-id != -1] [
    let my-cluster cluster-id
    let my-sub-id sub-cluster-id
    let diff-sub-neighbors (turtles-on neighbors) with [cluster-id = my-cluster and sub-cluster-id != my-sub-id and sub-cluster-id != -1]
    ask diff-sub-neighbors [
      let dist calculate-psychDist-between myself self
      set between-same-parent-distances lput dist between-same-parent-distances
    ]
  ]

  ;; Between different parent clusters
  let between-diff-parent-distances []
  ask turtles with [cluster-id != -1] [
    let my-cluster cluster-id
    let diff-cluster-neighbors (turtles-on neighbors) with [cluster-id != my-cluster and cluster-id != -1]
    ask diff-cluster-neighbors [
      let dist calculate-psychDist-between myself self
      set between-diff-parent-distances lput dist between-diff-parent-distances
    ]
  ]

  print ""
  if length within-sub-distances > 0 [
    print (word "Within Sub-Cluster psychDist: " precision (mean within-sub-distances) 2 " (n=" (length within-sub-distances) ")")
  ]

  if length between-same-parent-distances > 0 [
    print (word "Between Sub-Clusters (same parent): " precision (mean between-same-parent-distances) 2 " (n=" (length between-same-parent-distances) ")")
  ]

  if length between-diff-parent-distances > 0 [
    print (word "Between Different Parent Clusters: " precision (mean between-diff-parent-distances) 2 " (n=" (length between-diff-parent-distances) ")")
  ]

  print ""
end

to analyze-clusters
  ;; Only analyze clusters with 4+ agents
  let cluster-ids remove-duplicates [cluster-id] of turtles with [cluster-id != -1]
  let valid-clusters []

  ;; Filter for clusters with 4+ agents
  foreach sort cluster-ids [id ->
    let cluster-size count turtles with [cluster-id = id]
    if cluster-size >= 4 [
      set valid-clusters lput id valid-clusters
    ]
  ]

  if length valid-clusters = 0 [
    print "No clusters with 4+ agents found."
    stop
  ]

  print "=========================================="
  print "CLUSTER ANALYSIS (4+ agents minimum)"
  print "=========================================="
  print ""

  ;; Analyze each valid cluster
  foreach valid-clusters [id ->
    analyze-single-cluster id
  ]

  ;; Compare within vs between cluster psychDist
  compare-cluster-psychDist valid-clusters
end

to analyze-single-cluster [id]
  let cluster-members turtles with [cluster-id = id]
  let cluster-size count cluster-members

  print (word "CLUSTER " id " (" cluster-size " agents)")
  print ""

  ;; 1. Mean trait scores per cluster
  print "Mean Trait Scores:"
  let mean-o mean [openness] of cluster-members
  let mean-c mean [conscientiousness] of cluster-members
  let mean-e mean [extraversion] of cluster-members
  let mean-a mean [agreeableness] of cluster-members
  let mean-n mean [neuroticism] of cluster-members

  print (word "  Openness:           " precision mean-o 2)
  print (word "  Conscientiousness:  " precision mean-c 2)
  print (word "  Extraversion:       " precision mean-e 2)
  print (word "  Agreeableness:      " precision mean-a 2)
  print (word "  Neuroticism:        " precision mean-n 2)
  print ""

  ;; 2. Frequency distribution - % agents with each trait dominant
  print "Trait Dominance Frequency (% of agents):"
  let trait-dominant-counts [0 0 0 0 0]  ;; [O C E A N]

  ask cluster-members [
    let traits (list openness conscientiousness extraversion agreeableness neuroticism)
    let max-score max traits
    let dominant-trait-indices filter [i -> (item i traits) = max-score] (range 5)

    ;; For each dominant trait this agent has, increment count
    let idx 0
    repeat length trait-dominant-counts [
      if member? idx dominant-trait-indices [
        let old-count item idx trait-dominant-counts
        set trait-dominant-counts replace-item idx trait-dominant-counts (old-count + 1)
      ]
      set idx idx + 1
    ]
  ]

  let trait-names ["Openness" "Conscientiousness" "Extraversion" "Agreeableness" "Neuroticism"]
  let idx 0
  foreach trait-dominant-counts [trait-count ->
    let pct (trait-count / cluster-size) * 100
    print (word "  " (item idx trait-names) ": " precision pct 1 "%")
    set idx idx + 1
  ]
  print ""

  ;; 3. Dominant trait per cluster (with tie-breaking)
  print "Cluster Dominant Trait (majority rule with tie-breaking):"
  let max-count max trait-dominant-counts
  let dominant-trait-indices filter [i -> (item i trait-dominant-counts) = max-count] (range 5)

  ifelse length dominant-trait-indices = 1 [
    print (word "  " (item (item 0 dominant-trait-indices) trait-names))
  ] [
    ;; Tie exists - report all tied traits
    print (word "  TIE: Multiple traits dominant")
  ]
  print ""

  ;; 4. Within-cluster psychDist
  let within-cluster-distances []
  ask cluster-members [
    let my-cluster-id cluster-id
    let my-neighbors (turtles-on neighbors) with [cluster-id = my-cluster-id]
    ask my-neighbors [
      let dist calculate-psychDist-between myself self
      set within-cluster-distances lput dist within-cluster-distances
    ]
  ]

  ifelse length within-cluster-distances > 0 [
    let mean-within-dist mean within-cluster-distances
    print (word "Within-Cluster Mean psychDist: " precision mean-within-dist 2)
  ] [
    print (word "Within-Cluster Mean psychDist: N/A (no in-cluster neighbors)")
  ]
  print ""
  print "=========================================="
  print ""
end

to compare-cluster-psychDist [valid-clusters]
  print "BETWEEN-CLUSTER vs WITHIN-CLUSTER psychDist COMPARISON"
  print "=========================================="

  ;; Calculate all within-cluster distances
  let all-within-distances []
  ask turtles with [cluster-id != -1] [
    let my-cluster cluster-id
    let my-neighbors (turtles-on neighbors) with [cluster-id = my-cluster]
    ask my-neighbors [
      let dist calculate-psychDist-between myself self
      set all-within-distances lput dist all-within-distances
    ]
  ]

  ;; Calculate all between-cluster distances
  let all-between-distances []
  ask turtles with [cluster-id != -1] [
    let my-cluster cluster-id
    let between-neighbors (turtles-on neighbors) with [cluster-id != my-cluster and cluster-id != -1]
    ask between-neighbors [
      let dist calculate-psychDist-between myself self
      set all-between-distances lput dist all-between-distances
    ]
  ]

  print ""
  ifelse length all-within-distances > 0 [
    let mean-within mean all-within-distances
    let min-within min all-within-distances
    let max-within max all-within-distances
    print (word "Within-Cluster psychDist:")
    print (word "  Mean: " precision mean-within 2)
    print (word "  Min:  " precision min-within 2)
    print (word "  Max:  " precision max-within 2)
  ] [
    print (word "Within-Cluster psychDist: No in-cluster neighbors found")
  ]

  print ""
  ifelse length all-between-distances > 0 [
    let mean-between mean all-between-distances
    let min-between min all-between-distances
    let max-between max all-between-distances
    print (word "Between-Cluster psychDist:")
    print (word "  Mean: " precision mean-between 2)
    print (word "  Min:  " precision min-between 2)
    print (word "  Max:  " precision max-between 2)
  ] [
    print (word "Between-Cluster psychDist: No between-cluster neighbors found")
  ]

  print "=========================================="
end









;to analyze-clusters
;  print "===== CLUSTER ANALYSIS ====="
;  print ""
;
;  let cluster-ids remove-duplicates [cluster-id] of turtles with [cluster-id != -1]
;
;  foreach sort cluster-ids [
;    id ->
;    analyze-single-cluster id
;  ]
;
;  print ""
;  print "===== BETWEEN-CLUSTER & WITHIN-CLUSTER HOMOGENEITY ====="
;  calculate-between-cluster-homogeneity cluster-ids
;end

;to analyze-single-cluster [id]
;  let cluster-members turtles with [cluster-id = id]
;  let cluster-size count cluster-members
;
;  print (word "CLUSTER " id " (" cluster-size " agents)")
;  print "---"
;
;  ;; 2. Cluster density
;  let cluster-bounds get-cluster-bounds cluster-members
;  let area (item 0 cluster-bounds) * (item 1 cluster-bounds)
;  let density ifelse-value (area > 0) [cluster-size / area] [0]
;  print (word "  Density: " precision density 3 " agents/patch")
;
;  ;; 4 & 5. Average traits and diversity (std dev)
;  let o-vals [openness] of cluster-members
;  let c-vals [conscientiousness] of cluster-members
;  let e-vals [extraversion] of cluster-members
;  let a-vals [agreeableness] of cluster-members
;  let n-vals [neuroticism] of cluster-members
;
;  print "  Avg Traits (Mean ± Std Dev):"
;  print (word "    Openness: " precision (mean o-vals) 2 " ± " precision (std-dev o-vals) 2)
;  print (word "    Conscientiousness: " precision (mean c-vals) 2 " ± " precision (std-dev c-vals) 2)
;  print (word "    Extraversion: " precision (mean e-vals) 2 " ± " precision (std-dev e-vals) 2)
;  print (word "    Agreeableness: " precision (mean a-vals) 2 " ± " precision (std-dev a-vals) 2)
;  print (word "    Neuroticism: " precision (mean n-vals) 2 " ± " precision (std-dev n-vals) 2)
;
;  ;; 6. Dominant personality type
;  let trait-means (list (mean o-vals) (mean c-vals) (mean e-vals) (mean a-vals) (mean n-vals))
;  let trait-names ["Openness" "Conscientiousness" "Extraversion" "Agreeableness" "Neuroticism"]
;  let dominant-index 0
;  let max-trait item 0 trait-means
;  let i 0
;  repeat length trait-means [
;    if item i trait-means > max-trait [
;      set max-trait item i trait-means
;      set dominant-index i
;    ]
;    set i i + 1
;  ]
;  print (word "  Dominant Trait: " item dominant-index trait-names " (" precision max-trait 2 ")")
;
;  ;; 7. Color composition
;  print "  Color Composition:"
;  let red-count count cluster-members with [color = red]
;  let orange-count count cluster-members with [color = orange]
;  let yellow-count count cluster-members with [color = yellow]
;  let green-count count cluster-members with [color = green]
;  let blue-count count cluster-members with [color = blue]
;
;  if cluster-size > 0 [
;    print (word "    Red (O): " red-count " (" precision (red-count / cluster-size * 100) 1 "%)")
;    print (word "    Orange (C): " orange-count " (" precision (orange-count / cluster-size * 100) 1 "%)")
;    print (word "    Yellow (E): " yellow-count " (" precision (yellow-count / cluster-size * 100) 1 "%)")
;    print (word "    Green (A): " green-count " (" precision (green-count / cluster-size * 100) 1 "%)")
;    print (word "    Blue (N): " blue-count " (" precision (blue-count / cluster-size * 100) 1 "%)")
;  ]
;
;  ;; 8. Within-cluster compatibility (average psychDist)
;  let within-cluster-distances get-within-cluster-distances cluster-members
;  if length within-cluster-distances > 0 [
;    let avg-within mean within-cluster-distances
;    print (word "  Within-Cluster Avg PsychDist: " precision avg-within 2)
;  ]
;
;  print ""
;end
;
;to-report get-cluster-bounds [cluster-members]
;  let x-coords [xcor] of cluster-members
;  let y-coords [ycor] of cluster-members
;  let x-range (max x-coords - min x-coords)
;  let y-range (max y-coords - min y-coords)
;
;  ;; Avoid division by zero
;  if x-range = 0 [ set x-range 1 ]
;  if y-range = 0 [ set y-range 1 ]
;
;  report (list x-range y-range)
;end
;
;to-report get-within-cluster-distances [cluster-members]
;  let distances []
;  let members-list sort cluster-members
;  let n length members-list
;
;  if n < 2 [ report distances ]
;
;  let i 0
;  repeat n [
;    let j i + 1
;    repeat (n - j) [
;      let agent1 item i members-list
;      let agent2 item j members-list
;      let dist calculate-psychDist-between agent1 agent2
;      set distances lput dist distances
;      set j j + 1
;    ]
;    set i i + 1
;  ]
;
;  report distances
;end
;
;to calculate-between-cluster-homogeneity [cluster-ids]
;  let cluster-trait-avgs []
;
;  ;; Collect average traits for each cluster
;  foreach cluster-ids [
;    id ->
;    let cluster-members turtles with [cluster-id = id]
;    let o-avg mean [openness] of cluster-members
;    let c-avg mean [conscientiousness] of cluster-members
;    let e-avg mean [extraversion] of cluster-members
;    let a-avg mean [agreeableness] of cluster-members
;    let n-avg mean [neuroticism] of cluster-members
;
;    set cluster-trait-avgs lput (list o-avg c-avg e-avg a-avg n-avg) cluster-trait-avgs
;  ]
;
;  ;; Compare clusters pairwise
;  if length cluster-ids > 1 [
;    print "Between-Cluster Differences (avg trait distance):"
;    let k 0
;    repeat length cluster-ids [
;      let m k + 1
;      repeat (length cluster-ids - m) [
;        let traits1 item k cluster-trait-avgs
;        let traits2 item m cluster-trait-avgs
;
;        let diff 0
;        let n 0
;        repeat 5 [
;          set diff diff + abs(item n traits1 - item n traits2)
;          set n n + 1
;        ]
;        set diff diff / 5
;
;        print (word "  Cluster " k " vs Cluster " m ": " precision diff 2)
;        set m m + 1
;      ]
;      set k k + 1
;    ]
;  ]
;
;  ;; Within-cluster homogeneity summary
;  print ""
;  print "Within-Cluster Homogeneity (lower = more homogeneous):"
;  foreach cluster-ids [
;    id ->
;    let cluster-members turtles with [cluster-id = id]
;    let o-std std-dev [openness] of cluster-members
;    let c-std std-dev [conscientiousness] of cluster-members
;    let e-std std-dev [extraversion] of cluster-members
;    let a-std std-dev [agreeableness] of cluster-members
;    let n-std std-dev [neuroticism] of cluster-members
;
;    let avg-std (o-std + c-std + e-std + a-std + n-std) / 5
;    print (word "  Cluster " id ": " precision avg-std 2 " (avg std dev across traits)")
;  ]
;end
;
;to-report std-dev [values]
;  if length values < 2 [ report 0 ]
;  let avg mean values
;  let sum-sq 0
;  foreach values [
;    v ->
;    set sum-sq sum-sq + ((v - avg) ^ 2)
;  ]
;  report sqrt (sum-sq / (length values - 1))
;end
@#$#@#$#@
GRAPHICS-WINDOW
335
15
700
381
-1
-1
7.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

MONITOR
565
410
690
455
Percent Unhappy
percent-unhappy
1
1
11

MONITOR
408
410
538
455
Percent Incompatible
percent-incompatible
1
1
11

PLOT
72
145
321
288
Percent Incompatible
time
%
0.0
25.0
0.0
100.0
true
false
"" ""
PENS
"percent" 1.0 0 -2674135 true "" "plot percent-incompatible"

PLOT
72
289
321
453
Percent Unhappy
time
%
0.0
25.0
0.0
100.0
true
false
"" ""
PENS
"percent" 1.0 0 -10899396 true "" "plot percent-unhappy"

SLIDER
77
60
302
93
number
number
500
2500
500.0
10
1
NIL
HORIZONTAL

SLIDER
77
100
329
133
%-compatible-neighbors-wanted
%-compatible-neighbors-wanted
0.0
100.0
35.0
5
1
%
HORIZONTAL

BUTTON
96
19
176
52
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
186
19
266
52
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
72
510
314
543
pairwise-incompatibility-threshold
pairwise-incompatibility-threshold
0
500
120.0
5
1
NIL
HORIZONTAL

SWITCH
85
565
270
598
use-local-search
use-local-search
1
1
-1000

SWITCH
85
615
272
648
use-dualLevel-calc-A_N
use-dualLevel-calc-A_N
1
1
-1000

MONITOR
405
465
600
510
NIL
psychDist-summary
1
1
11

BUTTON
405
525
582
558
NIL
check-correlations-simple
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
402
575
587
608
NIL
trait-and-color-summary
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
630
470
782
530
cluster-ID-Input
7.0
1
0
Number

BUTTON
630
545
807
578
Highlight Selected Cluster
highlight-cluster cluster-id-input
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
400
625
585
658
Cluster-Analysis
identify-spatial-clusters\ncolor-clusters\nanalyze-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
635
590
742
623
NIL
color-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
85
670
322
703
use-extreme-incompatibility-rule
use-extreme-incompatibility-rule
0
1
-1000

SLIDER
72
720
314
753
min-shared-neighbors
min-shared-neighbors
1
4
2.0
1
1
NIL
HORIZONTAL

SLIDER
72
760
314
793
min-sub-cluster-size
min-sub-cluster-size
2
10
3.0
1
1
NIL
HORIZONTAL

BUTTON
400
675
585
708
Identify Sub-Clusters
identify-sub-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
635
640
807
673
Color Sub-Clusters (New Colors)
color-sub-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
635
680
807
713
Color Sub-Clusters (Hierarchical)
color-sub-clusters-hierarchical
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
400
715
585
748
Analyze Sub-Clusters
analyze-sub-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## ACKNOWLEDGMENT

This model is from Chapter Three of the book "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

This model is in the IABM Textbook folder of the NetLogo Models Library. The model, as well as any updates to the model, can also be found on the textbook website: http://www.intro-to-abm.com/.

## WHAT IS IT?

This project models the behavior of turtles of different ethnicities in a mythical pond. All the turtles get along with each other. But each turtle wants to make sure that it lives near some of "its own." The simulation shows how these individual preferences ripple through the pond, leading to large-scale patterns.

This project was inspired by Thomas Schelling's writings about social systems (particularly with regards to housing segregation in cities).

This model is an extension of the Segregation Simple model. It allows you to set up more than 2 ethnicities of agents.

## HOW TO USE IT

Click the SETUP button to set up the turtles. There are equal numbers of each color turtles. The turtles move around until there is at most one turtle on a patch.  Click GO to start the simulation. If turtles don't have enough same-color neighbors, they jump to a nearby patch.

The NUMBER slider controls the total number of turtles. (It takes effect the next time you click SETUP.) The NUMBER-OF-ETHNICITIES slider controls the number of different types of turtles, each a different color. The %-SIMILAR-WANTED slider controls the percentage of same-color turtles that each turtle wants among its neighbors. For example, if the slider is set at 30, each green turtle wants at least 30% of its neighbors to be green turtles.

The "PERCENT SIMILAR" monitor shows the average percentage of same-color neighbors for each turtle. It starts at about 0.5, since each turtle starts (on average) with an equal number of red and green turtles as neighbors. The "PERCENT UNHAPPY" monitor shows the percent of turtles that have fewer same-ethnicity neighbors than they want (and thus want to move).  Both monitors are also plotted.

## THINGS TO NOTICE

When you execute SETUP, the turtles are randomly distributed throughout the pond. But many turtles are "unhappy" since they don't have enough neighbors of the same ethnicity. The unhappy turtles jump to new locations in the vicinity. But in the new locations, they might tip the balance of the local population, prompting other turtles to leave. If a few red turtles move into an area, the local blue or orange turtles might leave. But when the blue or orange turtles move to a new area, they might prompt red turtles to leave that area, and so on.

Over time, the number of unhappy turtles decreases. But the pond becomes more segregated, with clusters of each ethnicity.

Again, relatively small individual preferences can lead to significant overall segregation. The exact numbers depend on how many ethnicities you have, and on the random distribution of their preferences for similarity.

## THINGS TO TRY

Try different values for %-SIMILAR-WANTED. How does the overall degree of segregation change?

Try different values for NUMBER-OF-ETHNICITIES. Does the overall segregation pattern change? How so, and why?

If each turtle wants at least 40% same-ethnicity neighbors, what percentage (on average) do they end up with?

## NETLOGO FEATURES

In the UPDATE-GLOBALS procedure, note the use of SUM, COUNT and WITH to compute the percentages displayed in the monitors and plots.

## CREDITS AND REFERENCES

This model is a simplified version of:

* Wilensky, U. (1997).  NetLogo Segregation model.  http://ccl.northwestern.edu/netlogo/models/Segregation.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Schelling, T. (1978). Micromotives and Macrobehavior. New York: Norton.

See also: Rauch, J. (2002). Seeing Around Corners; The Atlantic Monthly; April 2002;Volume 289, No. 4; 35-48. https://www.theatlantic.com/magazine/archive/2002/04/seeing-around-corners/302471/

## HOW TO CITE

This model is part of the textbook, “Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo.”

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U., Rand, W. (2006).  NetLogo Segregation Simple Extension 1 model.  http://ccl.northwestern.edu/netlogo/models/SegregationSimpleExtension1.  Center for Connected Learning and Computer-Based Modeling, Northwestern Institute on Complex Systems, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the textbook as:

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## COPYRIGHT AND LICENSE

Copyright 2006 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2006 Cite: Wilensky, U., Rand, W. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
