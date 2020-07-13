// Created by iWeb 2.0.3 local-build-20080505

setTransparentGifURL('Media/transparent.gif');function applyEffects()
{var registry=IWCreateEffectRegistry();registry.registerEffects({shadow_1:new IWShadow({blurRadius:10,offset:new IWPoint(3.5355,3.5355),color:'#000000',opacity:0.800000}),shadow_0:new IWShadow({blurRadius:10,offset:new IWPoint(4.9497,4.9497),color:'#000000',opacity:0.800000}),stroke_0:new IWStrokeParts([{rect:new IWRect(-1,1,2,298),url:'Introduction_files/stroke.png'},{rect:new IWRect(-1,-1,2,2),url:'Introduction_files/stroke_1.png'},{rect:new IWRect(1,-1,187,2),url:'Introduction_files/stroke_2.png'},{rect:new IWRect(188,-1,2,2),url:'Introduction_files/stroke_3.png'},{rect:new IWRect(188,1,2,298),url:'Introduction_files/stroke_4.png'},{rect:new IWRect(188,299,2,2),url:'Introduction_files/stroke_5.png'},{rect:new IWRect(1,299,187,2),url:'Introduction_files/stroke_6.png'},{rect:new IWRect(-1,299,2,2),url:'Introduction_files/stroke_7.png'}],new IWSize(189,300)),reflection_0:new IWReflection({opacity:0.16,offset:1.12})});registry.applyEffects();}
function hostedOnDM()
{return false;}
function onPageLoad()
{loadMozillaCSS('Introduction_files/IntroductionMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');adjustLineHeightIfTooBig('id2');adjustFontSizeIfTooBig('id2');Widget.onload();fixupAllIEPNGBGs();fixAllIEPNGs('Media/transparent.gif');applyEffects()}
function onPageUnload()
{Widget.onunload();}
