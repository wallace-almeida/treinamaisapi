package com.autosenseapi.controller;

import com.autosenseapi.controller.swagger.UserControllerSwagger;

import org.springframework.http.MediaType;

import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping(path = "/api/", produces = MediaType.APPLICATION_JSON_VALUE)
public class UserController implements UserControllerSwagger {


}
