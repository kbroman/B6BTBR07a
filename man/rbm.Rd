\name{rbm}
\alias{rbm}
\alias{rbm.pheno}
\docType{data}
\title{Data outsourced to RBM.}
\description{
See Mark Keller for description.
}
\usage{
data(rbm)
data(rbm.pheno)
}
\format{
  A data frame with 553 observations on the following 75 variables.
  \describe{
    \item{\code{US.barcode}}{US bar code?}
    \item{\code{Barcode}}{Bar code number.}
    \item{\code{sample.id}}{Sample ID.}
    \item{\code{mouse.id}}{Mouse ID as \code{Mousennnn}}
    \item{\code{strain}}{a factor with levels \code{F2}}
    \item{\code{sex}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Apo.A1..Apolipoprotein.A1.}}{a numeric vector}
    \item{\code{Beta.2.Microglobulin}}{a numeric vector}
    \item{\code{Calbindin}}{a numeric vector}
    \item{\code{CD40}}{a numeric vector}
    \item{\code{CD40.Ligand}}{a numeric vector}
    \item{\code{Clusterin}}{a numeric vector}
    \item{\code{CRP..C.Reactive.Protein.}}{a numeric vector}
    \item{\code{Cystatin.C}}{a numeric vector}
    \item{\code{EGF..Epidermal.Growth.Factor.}}{a numeric vector}
    \item{\code{Endothelin.1}}{a numeric vector}
    \item{\code{Eotaxin}}{a numeric vector}
    \item{\code{Factor.VII}}{a numeric vector}
    \item{\code{FGF.9..Fibroblast.Growth.Factor.9.}}{a numeric vector}
    \item{\code{FGF.basic..Fibroblast.Growth.Factor.basic.}}{a numeric vector}
    \item{\code{Fibrinogen}}{a numeric vector}
    \item{\code{GCP.2..Granulocyte.Chemotactic.Protein.2.}}{a numeric vector}
    \item{\code{GM.CSF..Granulocyte.Macrophage.Colony.Stimulating.Factor.}}{a numeric vector}
    \item{\code{Growth.Hormone}}{a numeric vector}
    \item{\code{GST.alpha..Glutathione.S.Transferase.alpha.}}{a numeric vector}
    \item{\code{GST.Mu}}{a numeric vector}
    \item{\code{Haptoglobin}}{a numeric vector}
    \item{\code{IFN.gamma..Interferon.gamma.}}{a numeric vector}
    \item{\code{IgA..Immunoglobulin.A.}}{a numeric vector}
    \item{\code{IL.10..Interleukin.10.}}{a numeric vector}
    \item{\code{IL.11..Interleukin.11.}}{a numeric vector}
    \item{\code{IL.12p70..Interleukin.12p70.}}{a numeric vector}
    \item{\code{IL.17..Interleukin.17.}}{a numeric vector}
    \item{\code{IL.18..Interleukin.18.}}{a numeric vector}
    \item{\code{IL.1alpha..Interleukin.1alpha.}}{a numeric vector}
    \item{\code{IL.1beta..Interleukin.1beta.}}{a numeric vector}
    \item{\code{IL.2..Interleukin.2.}}{a numeric vector}
    \item{\code{IL.3..Interleukin.3.}}{a numeric vector}
    \item{\code{IL.4..Interleukin.4.}}{a numeric vector}
    \item{\code{IL.5..Interleukin.5.}}{a numeric vector}
    \item{\code{IL.6..Interleukin.6.}}{a numeric vector}
    \item{\code{IL.7..Interleukin.7.}}{a numeric vector}
    \item{\code{Insulin}}{a numeric vector}
    \item{\code{IP.10..Inducible.Protein.10.}}{a numeric vector}
    \item{\code{KC.GROalpha..Melanoma.Growth.Stimulatory.Activity.Protein.}}{a numeric vector}
    \item{\code{Leptin}}{a numeric vector}
    \item{\code{LIF..Leukemia.Inhibitory.Factor.}}{a numeric vector}
    \item{\code{Lymphotactin}}{a numeric vector}
    \item{\code{MCP.1..Monocyte.Chemoattractant.Protein.1.}}{a numeric vector}
    \item{\code{MCP.3..Monocyte.Chemoattractant.Protein.3.}}{a numeric vector}
    \item{\code{MCP.5..Monocyte.Chemoattractant.Protein.5.}}{a numeric vector}
    \item{\code{M.CSF..Macrophage.Colony.Stimulating.Factor.}}{a numeric vector}
    \item{\code{MDC..Macrophage.Derived.Chemokine.}}{a numeric vector}
    \item{\code{MIP.1alpha..Macrophage.Inflammatory.Protein.1alpha.}}{a numeric vector}
    \item{\code{MIP.1beta..Macrophage.Inflammatory.Protein.1beta.}}{a numeric vector}
    \item{\code{MIP.1gamma..Macrophage.Inflammatory.Protein.1gamma.}}{a numeric vector}
    \item{\code{MIP.2..Macrophage.Inflammatory.Protein.2.}}{a numeric vector}
    \item{\code{MIP.3beta..Macrophage.Inflammatory.Protein.3beta.}}{a numeric vector}
    \item{\code{MMP.9..Matrix.Metalloproteinase.9.}}{a numeric vector}
    \item{\code{MPO..Myeloperoxidase.}}{a numeric vector}
    \item{\code{Myoglobin}}{a numeric vector}
    \item{\code{NGAL..Lipocalin.2.}}{a numeric vector}
    \item{\code{OSM..Oncostatin.M.}}{a numeric vector}
    \item{\code{Osteopontin}}{a numeric vector}
    \item{\code{RANTES..Regulation.Upon.Activation..Normal.T.Cell.Expressed.and.Secreted.}}{a numeric vector}
    \item{\code{SAP..Serum.Amyloid.P.}}{a numeric vector}
    \item{\code{SCF..Stem.Cell.Factor.}}{a numeric vector}
    \item{\code{SGOT..Serum.Glutamic.Oxaloacetic.Transaminase.}}{a numeric vector}
    \item{\code{TIMP.1..Tissue.Inhibitor.of.Metalloproteinase.Type.1.}}{a numeric vector}
    \item{\code{Tissue.Factor}}{a numeric vector}
    \item{\code{TNF.alpha..Tumor.Necrosis.Factor.alpha.}}{a numeric vector}
    \item{\code{TPO..Thrombopoietin.}}{a numeric vector}
    \item{\code{VCAM.1..Vascular.Cell.Adhesion.Molecule.1.}}{a numeric vector}
    \item{\code{VEGF..Vascular.Endothelial.Cell.Growth.Factor.}}{a numeric vector}
    \item{\code{vWF..von.Willebrand.Factor.}}{a numeric vector}
  }
}
\examples{
data(rbm)
## maybe str(rbm) ; plot(rbm) ...
}
\keyword{datasets}
