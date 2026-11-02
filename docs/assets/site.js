(function(){
  document.querySelectorAll('pre').forEach(function(p){
    var b=document.createElement('button');b.className='copy';b.textContent='Copy';
    b.onclick=function(){navigator.clipboard&&navigator.clipboard.writeText(p.innerText.replace(/^Copy\n?/,'')).then(function(){b.textContent='Copied';setTimeout(function(){b.textContent='Copy'},1200)})};
    p.appendChild(b);
  });
  var q=document.getElementById('q'),r=document.getElementById('results');
  if(q&&window.SEARCH_INDEX){
    q.addEventListener('input',function(){
      var t=q.value.trim().toLowerCase();r.innerHTML='';if(t.length<2)return;
      window.SEARCH_INDEX.filter(function(e){return (e.title+' '+e.text).toLowerCase().indexOf(t)>-1}).slice(0,8).forEach(function(e){
        var li=document.createElement('li'),a=document.createElement('a');a.href=e.url;a.textContent=e.title+(e.section?' – '+e.section:'');li.appendChild(a);r.appendChild(li);});
      if(!r.children.length){var li=document.createElement('li');li.style.color='#5b6770';li.textContent='No matches';r.appendChild(li);}
    });
  }
})();
