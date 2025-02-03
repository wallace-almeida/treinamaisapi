package com.adontoApi.controller.swagger;

import java.util.List;

import com.adontoApi.entity.User;
import com.adontoApi.entity.dto.SignUpRequest;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;


import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseStatus;

@Tag(name = "User", description = "Usuarios do sistema")
public interface UserControllerSwagger {


	@GetMapping("/allUser")
	List<User> findAll();


	//	//BLOCO POST
	@PostMapping("/signUp")
	@ResponseStatus(code = HttpStatus.CREATED)
	User create(@RequestBody @Valid SignUpRequest signUpRequest);

}
