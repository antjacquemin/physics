// Created by iWeb 2.0.3 local-build-20080505

setTransparentGifURL('Media/transparent.gif');function applyEffects()
{var registry=IWCreateEffectRegistry();registry.registerEffects({stroke_0:new IWStrokeParts([{rect:new IWRect(-2,2,4,296),url:'Accueil_files/stroke.png'},{rect:new IWRect(-2,-2,4,4),url:'Accueil_files/stroke_1.png'},{rect:new IWRect(2,-2,295,4),url:'Accueil_files/stroke_2.png'},{rect:new IWRect(297,-2,5,4),url:'Accueil_files/stroke_3.png'},{rect:new IWRect(297,2,5,296),url:'Accueil_files/stroke_4.png'},{rect:new IWRect(297,298,5,4),url:'Accueil_files/stroke_5.png'},{rect:new IWRect(2,298,295,4),url:'Accueil_files/stroke_6.png'},{rect:new IWRect(-2,298,4,4),url:'Accueil_files/stroke_7.png'}],new IWSize(299,300)),shadow_0:new IWShadow({blurRadius:19,offset:new IWPoint(7.7782,7.7782),color:'#000000',opacity:0.810000})});registry.applyEffects();}
function hostedOnDM()
{return false;}
function onPageLoad()
{loadMozillaCSS('Accueil_files/AccueilMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');Widget.onload();fixupAllIEPNGBGs();fixAllIEPNGs('Media/transparent.gif');applyEffects()}
function onPageUnload()
{Widget.onunload();}
