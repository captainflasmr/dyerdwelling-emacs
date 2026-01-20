---
title: "New Package arscript-mode"
author: ["James Dyer"]
lastmod: 2024-09-07T08:55:00+01:00
tags: ["package", "emacs", "artrage", "arscript", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240906074332-emacs--New-Package-arscript-mode.jpg"
---

As an avid ArtRage user for almost 10 years I have in the past delved into the subtle art of editing arscript files:

<!--more-->

{{< figure src="/emacs/20240906074332-emacs--New-Package-arscript-mode.jpg" width="100%" >}}

> ArtRage arscript files are specialized script files used by the ArtRage painting software to automate and record various painting actions and effects. These files encapsulate sequences of drawing commands, tool selections, color settings, and other actions that can be replayed to recreate the painting process. The arscript format includes structured elements such as keywords, modifiers, structured tags, and numerical values, allowing for detailed specification of both high-level actions and granular details like brush strokes and layer manipulations. The format aims to facilitate complex artistic workflows by enabling repeatable and shareable painting routines, making it an essential tool for digital artists using ArtRage.

They are designed to me modifiable, so what better way to modify them than with Emacs!

I created an `arscript` major mode with the idea to eventually add text manipulation functions to tweak a painting playback.


## Summary {#summary}

`arscript-mode` is an Emacs major mode designed to facilitate the editing of arscript files, providing syntax highlighting and other useful editing features tailored specifically for the arscript file format.


## Screenshot {#screenshot}

{{< figure src="/emacs/arscript-mode-00.jpg" width="100%" >}}


## Links {#links}

<https://www.artrage.com>

<https://www.artrage.com/manuals/scripts/>

<https://www.artrage.com/manuals/scripts/script-editing-tips/artrage-scripting-guide/>


## Whats New {#whats-new}


### v0.1.0 {#v0-dot-1-dot-0}

First version


## Features {#features}

-   Syntax highlighting for arscript-specific keywords, modifiers, and structures.
-   Custom indentation logic for arscript code blocks.
-   Auto-detection of .arscript files to automatically enable the mode.


## Installation {#installation}


### use-package (MELPA) {#use-package--melpa}

```elisp
(use-package arscript-mode)
```


### use-package (emacs 30+) directly from github {#use-package--emacs-30-plus--directly-from-github}

Put the following into your emacs init file:

```elisp
(use-package arscript-mode
  :vc (:fetcher github :repo "captainflasmr/arscript-mode"))
```


### From source {#from-source}

Download the \`.el\` file and place it in your Emacs \`load-path\` or in a specific source directory "~/source/repos/arscript-mode"

Then either manually load it or add it to your configuration to be loaded at startup.

```elisp
(require 'arscript-mode)
```

OR

```elisp
(use-package arscript-mode
  :load-path "~/source/repos/arscript-mode")
```


## Usage {#usage}

Opening any .arscript file with Emacs should automatically enable `arscript-mode`, providing you with syntax highlighting and indentation support for editing arscript files.

Example arscript file:

```arscript
//===========================================================================
//===========================================================================
//                            ArtRage Script File.
//===========================================================================
//===========================================================================


//===========================================================================
// Version Block - Script version and ArtRage version:
//===========================================================================

<Version>
   ArtRage Version: ArtRage 3 4
   ArtRage Build: 4.5.3
   Professional Edition: Yes
   Script Version: 1
</Version>


//===========================================================================
// Header block - Info about the painting/person who generated this script:
//===========================================================================

<Header>
   // === Project data
   Painting Name: "Willow"
   Painting Width: 2456
   Painting Height: 2206
   Painting DPI: 200
   Mask Edge Map Width: 1280
   Mask Edge Map Height: 800
   // === Author data
   Author Name: ""
   Script Name: ""
   Comment: ""
   Script Type: ""
   Script Feature Flags: 0x000000034
</Header>


//===========================================================================
// ArtRage project features. Sets the startup state of the script:
//===========================================================================

<StartupFeatures>
   Script Startup Features: {
   }// End of Script startup feature binary data.
</StartupFeatures>


//===========================================================================
// Script data follows:
//===========================================================================

<Events>
   Wait: 0.000s	EvType: Command	CommandID: CID_SetClearCanvas	ParamType: flag	Value: { true }
   Wait: 14.031s	EvType: Command	CommandID: LoadReferenceImage	Idx: 0	Reference Image: {
   }// End of reference image binary data.
   Wait: 2.694s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.182031, 0.2)	Size: (320, 236)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 0.682s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.179688, 0.45)	Size: (320, 236)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 2.298s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.180078, 0.45)	Size: (803, 600)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 2.678s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.279297, 0.395)	Size: (803, 600)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 7.316s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.278906, 0.395)	Size: (864, 646)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 12.245s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.278906, 0.395)	Size: (139, 100)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 0.000s	EvType: Command	CommandID: SetForeColour	ParamType: Pixel	Value: { 0x0FF7386A0 }
   Wait: 3.341s	EvType: Command	CommandID: CID_SetClearCanvas	ParamType: flag	Value: { true }
   Wait: 3.819s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.278906, 0.395)	Size: (684, 520)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 8.354s	EvType: Command	CommandID: ReferenceImageXForm	Idx: 0	Loc: (0.278906, 0.395)	Size: (161, 119)	Scale: 1	Rot: 0	Off: (0, 0)
   Wait: 1.659s	EvType: Command	CommandID: CanvasXForm	Scale: 0.0840503	Rot: 0	Off: (537, 307)
   Wait: 1.123s	EvType: Command	CommandID: CanvasXForm	Scale: 0.0840503	Rot: 0	Off: (497, 259)
   <StrokeEvent>
	   <StrokeHeader>
		   <EventPt>	Wait: 1.116s	Loc: (1054.6, 527.3)	Pr: 0.0521997	Ti: 0.489467	Ro: 1.27568	Fw: 1	Bt: 0	Rv: NO	Iv: NO	</EventPt>
		   <Recorded>	Yes	</Recorded>
		   <RandSeed>	0x000000000, 0x000000000	</RandSeed>
		   <Smooth>	Count:	3
				Loc: (1054.6, 527.3)	Pr: 0.132196	Ti: 1	Ro: 0	Fw: 1	Bt: 0
				Loc: (1086.56, 527.3)	Pr: 0.132196	Ti: 1	Ro: 0	Fw: 1	Bt: 0
				Loc: (1102.54, 527.3)	Pr: 0.132196	Ti: 1	Ro: 0	Fw: 1	Bt: 0
		   </Smooth>
		   <PrevA>	Loc: (-679.912, 774.652)	Pr: 0.135925	Ti: 1	Ro: 0	Fw: 1	Bt: 0	</PrevA>
		   <PrevB>	Loc: (-505.365, 600.105)	Pr: 0.174106	Ti: 1	Ro: 0	Fw: 1	Bt: 0	</PrevB>
		   <OldHd>	Loc: (1086.56, 527.3)	Pr: 0	Ti: 0.477897	Ro: 1.27511	Fw: 1	Bt: 0	Dr: (-0.919609, -0.392834)	Hd: (0.392834, -0.919609)	</OldHd>
		   <NewHd>	Loc: (1054.6, 527.3)	Pr: 0	Ti: 0.489467	Ro: 1.27568	Fw: 1	Bt: 0	Dr: (-0.99682, -0.0796825)	Hd: (0.0796825, -0.99682)	</NewHd>
	   </StrokeHeader>
	   Wait: 0.000s	Loc: (1054.6, 535.29)	Pr: 0.0569451	Ti: 0.489467	Ro: 1.27568	Fw: 1	Bt: 0	Rv: NO	Iv: NO
	   Wait: 0.002s	Loc: (1054.6, 543.279)	Pr: 0.0237271	Ti: 0.489467	Ro: 1.27568	Fw: 1	Bt: 0	Rv: NO	Iv: NO
   </StrokeEvent>
```


## Customization {#customization}

Currently, `arscript-mode` provides a basic set of features optimized for general usage. Future versions may include customizable options based on user feedback.


## Contributing {#contributing}

Contributions to `arscript-mode` are welcome! Whether it's bug reports, feature suggestions, or code contributions, feel free to reach out or submit pull requests on GitHub.


## ROADMAP {#roadmap}


### <span class="org-todo todo TODO">TODO</span> review syntactical keyword highlighting based on contents of arscript pdf manual {#review-syntactical-keyword-highlighting-based-on-contents-of-arscript-pdf-manual}


### <span class="org-todo todo TODO">TODO</span> add text transformation functions to affect arscript and hence ArtRage playback {#add-text-transformation-functions-to-affect-arscript-and-hence-artrage-playback}


## Alternatives {#alternatives}

As far as I can tell this is the first Emacs major mode supporting the arscript format.
