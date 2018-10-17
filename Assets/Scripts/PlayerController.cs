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
    float viewRangeMin = 5;
    float viewRangeMax = 85;
    Vector3 curRot;

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

        float mouseY = -Input.GetAxis("Mouse Y");

        lookRot = new Vector3 (mouseY, 0, 0) * lookSensitivity;

        curRot = rotatable.transform.eulerAngles;

        curRot += lookRot;

        if(curRot.x >= 80 || curRot.x <= 10)
        {

            return;

        }

        rotatable.transform.Rotate (lookRot);

        transform.Rotate (0, Input.GetAxis("Mouse X") * lookSensitivity, 0);

        toRotate = rotatable.transform.rotation.eulerAngles;

        toRotate.z = 0;

        //rotatable.transform.localEulerAngles = new Vector3(Mathf.Clamp(rotatable.transform.localEulerAngles.x, 10, 80), rotatable.transform.localEulerAngles.y, 0);
        //toRotate.x = Mathf.Clamp(toRotate.x, 10, 80);
        //Debug.Log(rotatable.transform.rotation.eulerAngles.x + " | " + rotatable.transform.localEulerAngles.x);

        //rotatable.transform.rotation = Quaternion.Euler(Mathf.Clamp(toRotate.x, viewRangeMin, viewRangeMax), toRotate.y, toRotate.z);

    }

}
