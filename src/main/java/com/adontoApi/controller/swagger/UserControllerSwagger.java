package com.adontoApi.controller.swagger;

import com.adontoApi.entity.User;
import com.adontoApi.entity.dto.SignUpRequest;
import com.adontoApi.entity.dto.allUserDto;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "User", description = "Usuarios do sistema")
public interface UserControllerSwagger {


	@GetMapping("/allUser")
	ResponseEntity<Page<allUserDto>> findAll(
			@RequestParam(defaultValue = "0") int page,
			@RequestParam(defaultValue = "10") int size,
			@RequestParam(defaultValue = "name") String sortBy
	);

	//	//BLOCO POST
	@PostMapping("/signUp")
	@ResponseStatus(code = HttpStatus.CREATED)
	User create(@RequestBody @Valid SignUpRequest signUpRequest);

}
