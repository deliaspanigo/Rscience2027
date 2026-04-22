// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}




#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "a4",
  margin: (x: 1.5cm,y: 2cm,),
  numbering: "1",
  columns: 1,
)

#show: doc => article(
  title: [Rscience - Anova - script01],
  subtitle: [Import and control],
  authors: (
    ( name: [David Elías Panigo],
      affiliation: [],
      email: [] ),
    ),
  date: [2026-04-22],
  sectionnumbering: "1.1.a",
  toc_title: [Table of contents],
  toc_depth: 3,
  doc,
)

#pagebreak()
#outline(indent: auto, title: "Índice")
#pagebreak()
= Summary Anova
<summary-anova>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-8-1.png"))

= Tukey Table
<tukey-table>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-9-1.png"))

= Tukey plot
<tukey-plot>
#figure([
#box(image("grafico_final.png"))
], caption: figure.caption(
separator: "", 
position: bottom, 
[
#block[
]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-tukey-png>


= Frases Level 01
<frases-level-01>
The null hypothesis of normal distribution of residuals is not rejected.

The hypothesis of homogeneity of variances (homoscedasticity) is rejected.

Not all requirements for the model are met.

It is NOT valid to draw conclusions from the ANOVA test.

As the model requirements are not met, the ANOVA and Tukey analyses must be discarded, irrespective of the statistical values obtained. The literal and decontextualized interpretation of the ANOVA test and the Tukey test is detailed below for demonstrative purposes only, but holds no validity for drawing conclusions from them.

The null hypothesis of the ANOVA test is rejected. There are statistically significant differences in at least one level of the factor.

Since the ANOVA null hypothesis is rejected, the use of a multiple comparison test to accompany the ANOVA test is valid.

The Tukey test is selected as the multiple comparison test for this script.

The Tukey test indicates that there are at least 2 groups among the factor levels. The number of groups and their structure must be analyzed in detail using the Tukey table.

ANOVA indicates that all factor levels are equal, while Tukey has found at least 2 groups. Tukey is subject to the rejection of the ANOVA hypothesis. Since the ANOVA null hypothesis is not rejected, the Tukey test must not be taken into account for decision-making. All factor levels are equal.

= Frases Level 02
<frases-level-02>
The p value about normality distibution is 0.517664951040294. \
Alpha value is 0.05. \
The p value is equal or mayor than alpha value. \
The null hypothesis of normal distribution of residuals is not rejected. \
Los residuos poseen distribución normal.

The p value about homogeneity variances from residuals is 0.0150451770493758. \
Alpha value is 0.05. \
The p value is less than alpha value. \
The null hypothesis of homogeneity of variances from residuals is rejected. \
Los residuos son homogeneos.

Not all requirements for the model are met.

It is NOT valid to draw conclusions from the ANOVA test. This dataset must be analyzed with another statistical tool such as the Kruskal-Wallis test. If a more sophisticated tool is desired, the possibility of utilizing generalized linear mixed models, generalized linear models, generalized linear models, exact distribution linear models, etc., could be evaluated.

It is NOT valid to draw conclusions from the ANOVA test. As the model requirements are not met, the ANOVA and Tukey analyses must be discarded, irrespective of the statistical values obtained. The literal and decontextualized interpretation of the ANOVA test and the Tukey test is detailed below for demonstrative purposes only, but holds no validity for drawing conclusions from them.

The p-value for the ANOVA test is 4.97891917440023e-09. The alpha value is 0.05. The p-value is less than the alpha value. The null hypothesis of equal means for the ANOVA test is rejected. There are statistically significant differences in at least one level of the factor. By rejecting the null hypothesis, the ANOVA test guarantees statistically significant differences in at least one level of the factor with the lowest mean and the level of the factor with the highest mean.

Since the ANOVA null hypothesis is rejected, the use of a multiple comparison test to accompany the ANOVA test is valid.

The Tukey test is selected as the multiple comparison test for this script.

The Tukey test details at least 2 statistically different groups. The group structure detailed by the Tukey test must now be considered in order to provide a recommendation in your area of work.

Irrespective of Tukey's finding groups, since the ANOVA null hypothesis was not rejected, all factor levels are equal.

= Selected Case
<selected-case>
= DF case
<df-case>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-13-1.png"))

= DF selected case
<df-selected-case>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-14-1.png"))

= DF human
<df-human>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-15-1.png"))

When not all model requirements (Normality and/or Homogeneity) are met, the ANOVA analysis is immediately discarded. This invalidation occurs irrespective of whether the failure is due to non-normality of the residuals, non-homogeneity, or both requirements. Indistinctly of the statistical values obtained in the ANOVA test and the Tukey test, it is NOT valid to draw conclusions of any type; the entire analysis is completely discarded. An alternative statistical tool or path must be sought to analyze the data, with the quickest recommended tool being the Kruskal-Wallis test, although a better analysis may allow for the use of more robust tools such as Generalized Linear Mixed Models or Exact Statistics; the alternative statistical path is to perform a transformation on the response variable and attempt the 1-Factor ANOVA test again. The statistical options suggested must be verified and justified for their feasibility of use. Continuing with statistical interpretations without fulfilling the model's requirements is a serious error common among those new to statistics, and a common practice among those lacking the sufficient statistical knowledge to correctly analyze the reality they intend to study. If, despite all these warnings, you continue with the analysis, you expose your work to severe criticism from colleagues, evaluation committees, and publication reviewers.

= DF 02 - Requeriments
<df-02---requeriments>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-17-1.png"))

The ANOVA statistical test is based on 3 points for decision-making: shape, dispersion, and position. The shape is the normal distribution of the residuals. The dispersion is the homogeneous variances in the residuals, and the position is the mean of the response variable for one of the factor levels. By guaranteeing that the shape and dispersion of the residuals have a certain particular structure, ANOVA manages to test the position in the response variable. This is why the fulfillment of normality and homogeneity of variances of the residuals is a requirement, and only with the simultaneous fulfillment of both requirements is it valid to make an interpretation of the means. If all requirements are not met simultaneously, the ANOVA analysis must be discarded, irrespective of the results it may have yielded. There is no escape, and you must seek another statistical tool to interpret these results.

= DF 03 - Anova
<df-03---anova>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-19-1.png"))

The model requirements are not met. The ANOVA statistical test has the indispensable requirement of residual normality and homogeneity of variances. Both requirements must be met jointly for it to be valid to draw conclusions about the factor levels. Continuing with the statistical interpretation without fulfilling all the requirements is a catastrophic error. The ANOVA model requirements are neither optional nor can they be assumed. The model requirements are the fundamental basis upon which the ANOVA test relies to make decisions about the means of the factor levels. In this case, you must seek another statistical tool to perform the analysis of your data.

= DF 04 - Tukey
<df-04---tukey>
#box(image("report_pdf_files/figure-typst/unnamed-chunk-21-1.png"))

The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model requirements are not met, the ANOVA test is not valid, and therefore the Tukey test is not valid either. The information related to the Tukey test must be discarded. As detailed before, in this context, continuing with the statistical interpretation is a serious statistical error that demonstrates a total lack of statistical criteria for decision-making.
