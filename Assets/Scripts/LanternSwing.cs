using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LanternSwing : MonoBehaviour {

    public float randomness;
    public float x;
    public float y;
    public float z;
    
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        gameObject.GetComponent<Rigidbody>().AddForce(new Vector3(x, y, z) * Random.RandomRange(-randomness, randomness));

    }
    public void Swing()
    {
        
    }
}
