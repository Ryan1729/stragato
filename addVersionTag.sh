
tag=`sed -n 's/\"\(.*\)\"/\1/p' Version.elm | tr -d '[[:space:]]'`
if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
    echo "$tag already used. Not adding."
else
    echo "$tag not already used"
    git tag $tag && echo "added $tag" || (echo "error adding $tag" && exit 1)
fi
