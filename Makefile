BACKUP=/tmp/metaoku_$(shell date +"%Y-%m-%d-%T")

all:
	# # Save old pages/ if it exists
	[[ ! -d pages ]] || \
	( \
		mkdir ${BACKUP} && \
		mv pages ${BACKUP} && \
		echo "Moved old pages to ${BACKUP}/pages" \
	)
	# Save old config if it exists
	[[ ! -f config ]] || \
	( \
		mv config ${BACKUP}/config && \
		echo "Moved old config to ${BACKUP}/config" \
	)
	# Copy default pagesumentation to pages folder
	mkdir pages && cp inst/pages/* pages
	cp inst/config .

.PHONY: clean
clean:
	[[ ! -d pages  ]] || (mkdir -p ${BACKUP} && mv pages ${BACKUP})
	[[ ! -f config ]] || (mkdir -p ${BACKUP} && mv config ${BACKUP})
