using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour {

    public float speed;
    CharacterController cc;
    Vector3 movement = Vector3.zero;
    GameObject rotatable;
    public float gravity;
    public float lookSensitivity;
    Vector3 lookRot;
    Vector3 toRotate;
    Vector3 forward;

    private void Start ()
    {

        cc = GetComponent<CharacterController> ();
        rotatable = transform.GetChild(0).gameObject;
        
    }

    private void Update ()
    {
        /*
        movement.x = Input.GetAxisRaw ("Horizontal");
        movement.y = -gravity * Time.deltaTime;
        movement.z = Input.GetAxisRaw ("Vertical");
        */
        movement = transform.forward * Input.GetAxisRaw ("Vertical");
        movement += transform.right * Input.GetAxisRaw ("Horizontal");
        
        movement = movement.normalized;
        movement += -transform.up * gravity;
        //forward = transform.forward;
        movement *= speed * Time.deltaTime;
        cc.Move (movement);

        lookRot = new Vector3 (-Input.GetAxis ("Mouse Y"), 0, 0) * lookSensitivity;

        rotatable.transform.Rotate (lookRot);

        transform.Rotate (0, Input.GetAxis("Mouse X") * lookSensitivity, 0);

        toRotate = rotatable.transform.rotation.eulerAngles;

        toRotate.z = 0;

        rotatable.transform.rotation = Quaternion.Euler(toRotate);

    }

}
