
# Step to the child of the current commit.
step: 
	git checkout `git rev-list --topo-order HEAD..master | tail -1`

.PHONY: step
