using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour {

    public float speed;
    CharacterController cc;
    Vector3 movement;
    GameObject rotatable;
    public float gravity;
    public float lookSensitivity;
    Vector3 lookRot;
    Vector3 toRotate;

    private void Start ()
    {

        cc = GetComponent<CharacterController> ();
        rotatable = transform.GetChild(0).gameObject;
        
    }

    private void Update ()
    {

        movement.x = Input.GetAxisRaw ("Horizontal");
        movement.y = -gravity * Time.deltaTime;
        movement.z = Input.GetAxisRaw ("Vertical");

        movement = movement.normalized;
        movement *= speed * Time.deltaTime;
        cc.Move (movement);

        lookRot = new Vector3 (-Input.GetAxis ("Mouse Y"), Input.GetAxis ("Mouse X"), 0) * lookSensitivity;

        rotatable.transform.Rotate (lookRot);

        toRotate = rotatable.transform.rotation.eulerAngles;

        toRotate.z = 0;

        rotatable.transform.rotation = Quaternion.Euler(toRotate);

    }

}
