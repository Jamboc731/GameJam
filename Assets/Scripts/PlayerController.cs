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
    Camera cam;

    private void Start ()
    {

        cc = GetComponent<CharacterController> ();
        rotatable = transform.GetChild(0).gameObject;
        cam = rotatable.transform.GetChild(0).GetComponent<Camera>();

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

        if (Input.GetButtonDown("Fire1"))
        {

            Ray ray = new Ray(cam.transform.position, cam.transform.forward);

            RaycastHit hit;

            if(Physics.Raycast(ray, out hit))
            {

                if (hit.transform.gameObject.name.Contains("Skel"))
                {

                    Destroy(hit.transform.gameObject);

                }

            }

        }

    }

}
