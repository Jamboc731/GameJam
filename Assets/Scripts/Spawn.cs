using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Spawn : MonoBehaviour {

    public GameObject Skel;
    

	// Use this for initialization
	void Start () {
        InvokeRepeating("spawn", 1f, 1f);
	}
	
	// Update is called once per frame
	void spawn () {
        GameObject instance;
        GameObject[] Bones;
        instance = Instantiate(Skel, transform.position, transform.rotation);
        Bones = instance.GetComponentsInChildren<GameObject>();
        Destroy(instance, 5f);

        for (int i = 0; i < Bones.Length; i++)
        {
            Bones[i].GetComponent<Rigidbody>().AddForce(transform.forward * 5000);
            Bones[i].GetComponent<Renderer>().material.SetFloat("_Dissolve", 1f);
        }
            
       
	}
}
