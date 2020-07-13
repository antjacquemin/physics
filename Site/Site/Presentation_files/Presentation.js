// Created by iWeb 2.0.3 local-build-20080505

setTransparentGifURL('Media/transparent.gif');function applyEffects()
{var registry=IWCreateEffectRegistry();registry.registerEffects({shadow_0:new IWShadow({blurRadius:10,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:0.800000}),shadow_3:new IWShadow({blurRadius:10,offset:new IWPoint(0.0000,0.0000),color:'#000000',opacity:0.750000}),shadow_1:new IWShadow({blurRadius:10,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:0.800000}),reflection_0:new IWReflection({opacity:0.22,offset:1.13}),shadow_2:new IWShadow({blurRadius:10,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:0.800000})});registry.applyEffects();}
function hostedOnDM()
{return false;}
function onPageLoad()
{loadMozillaCSS('Presentation_files/PresentationMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');Widget.onload();fixupAllIEPNGBGs();fixAllIEPNGs('Media/transparent.gif');IMpreload('Presentation_files','shapeimage_1','0');applyEffects()}
function onPageUnload()
{Widget.onunload();}
