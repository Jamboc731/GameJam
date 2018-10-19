using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Skel : MonoBehaviour {


    private float dissolverate = -1.5f;

    // Use this for initialization
    void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {

        
        float dis = dissolverate += 0.01f;

        Debug.Log(dis);
        Renderer[] Bones;
        Bones = gameObject.GetComponentsInChildren<Renderer>();
        for (int i = 0; i < Bones.Length; i++)
        {
            

            //  dissolverate += 0.01f;
           
            Bones[i].GetComponent<Renderer>().material.SetFloat("_Dissolve", dis);
        }
    }
}
