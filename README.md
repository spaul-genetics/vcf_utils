# vcf_utils

I will keep adding custom codes that I use while working with VCF files. 

## VCF Stats

`bcftools` produce vcfstats in a txt formated file. There is a function `plot_vcfstats` that can be used to produce figures and a pdf report which does not work if pdflatex is not installed. I have used rmarkdown to produce a html report from the vcfstats txt file. 

```
Rscript script_render_html.r input_file.txt output_file.txt
```
