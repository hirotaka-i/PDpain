# PDpain
This is a repo for an analysis of the RCT investigating the effect of duloxetine for patients with Parkinson's disease with pain. The study detail is provided [here](https://jrct.niph.go.jp/en-latest-detail/jRCTs061180028)

# Setting up the analytical environment (use docker)
docker run -d -p 8787:8787 -v /c/Users/Hirotaka/Downloads/PDpain:/home/rstudio/wkdir -e PASSWORD=test --name rStudioJ ymattu/mecab-d
