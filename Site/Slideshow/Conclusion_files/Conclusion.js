// Created by iWeb 2.0.3 local-build-20080505

setTransparentGifURL('Media/transparent.gif');function applyEffects()
{var registry=IWCreateEffectRegistry();registry.registerEffects({shadow_0:new IWShadow({blurRadius:30,offset:new IWPoint(10.6066,10.6066),color:'#000000',opacity:1.000000}),shadow_1:new IWShadow({blurRadius:10,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:0.800000}),reflection_0:new IWReflection({opacity:1.00,offset:-47.91})});registry.applyEffects();}
function hostedOnDM()
{return false;}
function onPageLoad()
{loadMozillaCSS('Conclusion_files/ConclusionMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');adjustLineHeightIfTooBig('id2');adjustFontSizeIfTooBig('id2');Widget.onload();fixupAllIEPNGBGs();fixAllIEPNGs('Media/transparent.gif');applyEffects()}
function onPageUnload()
{Widget.onunload();}
