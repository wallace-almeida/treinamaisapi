package com.adontoApi.repository;

import java.util.List;

import com.adontoApi.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;



import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<User, Long>, JpaSpecificationExecutor<User>,  PagingAndSortingRepository<User, Long>{

	//BLOCO GET
	@Query(value = "SELECT * FROM USERS ", nativeQuery = true)
	List<User> getListUser();

	boolean existsByEmail(String email);
	boolean existsByCpfUser(String cpfUser);


	 /* @Query(value =
	  "SELECT * FROM INTEGRACAO.T_SUP_INTEGRACAO where ID_INTEGRACAO = :integracao ",
	 nativeQuery = true)
      User getIntegracaoById(@Param(value =
	 "integracao") Long integracao);*/





}
