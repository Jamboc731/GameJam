using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Lighting : MonoBehaviour {

	// Use this for initialization
	void Start () {
        Invoke("UpdateLighting", 1f);
	}
	
	// Update is called once per frame
	void Update () {
        RenderSettings.ambientMode = UnityEngine.Rendering.AmbientMode.Trilight;
    }
}
