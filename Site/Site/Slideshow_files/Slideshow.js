// Created by iWeb 2.0.3 local-build-20080505

setTransparentGifURL('Media/transparent.gif');function applyEffects()
{var registry=IWCreateEffectRegistry();registry.registerEffects({shadow_0:new IWShadow({blurRadius:30,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:1.000000}),shadow_1:new IWShadow({blurRadius:10,offset:new IWPoint(0.0000,0.0000),color:'#000000',opacity:0.750000}),reflection_0:new IWReflection({opacity:0.50,offset:2.50}),stroke_0:new IWStrokeParts([{rect:new IWRect(-1,1,2,53),url:'Slideshow_files/stroke.png'},{rect:new IWRect(-1,-1,2,2),url:'Slideshow_files/stroke_1.png'},{rect:new IWRect(1,-1,53,2),url:'Slideshow_files/stroke_2.png'},{rect:new IWRect(54,-1,2,2),url:'Slideshow_files/stroke_3.png'},{rect:new IWRect(54,1,2,53),url:'Slideshow_files/stroke_4.png'},{rect:new IWRect(54,54,2,2),url:'Slideshow_files/stroke_5.png'},{rect:new IWRect(1,54,53,2),url:'Slideshow_files/stroke_6.png'},{rect:new IWRect(-1,54,2,2),url:'Slideshow_files/stroke_7.png'}],new IWSize(55,55))});registry.applyEffects();}
function hostedOnDM()
{return false;}
function onPageLoad()
{loadMozillaCSS('Slideshow_files/SlideshowMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');Widget.onload();fixupAllIEPNGBGs();fixAllIEPNGs('Media/transparent.gif');IMpreload('Slideshow_files','shapeimage_1','0');IMpreload('Slideshow_files','shapeimage_1','1');IMpreload('Slideshow_files','shapeimage_1','2');IMpreload('Slideshow_files','shapeimage_1','3');IMpreload('Slideshow_files','shapeimage_1','4');IMpreload('Slideshow_files','shapeimage_1','5');IMpreload('Slideshow_files','shapeimage_1','6');IMpreload('Slideshow_files','shapeimage_1','7');applyEffects()}
function onPageUnload()
{Widget.onunload();}
