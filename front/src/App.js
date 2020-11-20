
import './App.css';
import axios from 'axios';
import React, { useState, useEffect } from 'react';

function App() {
  const [response, setResponse] = useState('waiting');

  
  useEffect(() => { 
    async function fetchData (){
        
      
      var result =  await axios.get('http://localhost:4000/hello').then((response) => response);
      console.log(result);
      setResponse(result.data);

    }  
    fetchData();
    
  }, []);
  return (
    <div className="App">
      <h1>Response</h1>
      <p>{response.tag}</p>
    </div>
  );
}

export default App;
